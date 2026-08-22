% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission â Competence Reading
 *   domain: disaster risk management/institutional memory/civil defense
 *
 * SUMMARY:
 *   This is the competence reading of the preparedness_transmission kernel.
 *   It treats civil defense drills and inspections as a functioning
 *   coordination mechanism that maintains operational knowledge through
 *   generational turnover. Unlike the husk reading (memorial ritual) or
 *   hybrid reading (stratified decay), this reading asserts that inspectors
 *   recognize novel failure signatures and participants improvise
 *   effectively. The constraint coordinates inter-agency disaster response
 *   across time by re-validating capability through practice, keeping
 *   institutional memory embodied rather than documentary.
 *
 * KEY AGENTS:
 *   - Civil defense authority (agenda_setter/institutional): designs, mandates, and evaluates drill regimes; bears administrative cost and gains validated institutional function.
 *   - Response personnel (beneficiary/moderate): participate in drills, pay with time and stress, gain validated adaptive competence and career-embedded capability.
 *   - Inspection targets (beneficiary/organized): agencies and facilities subject to readiness inspections; pay compliance burden, gain operational status and structured feedback.
 *   - Protected communities (beneficiary/powerless): populations in hazard zones receiving protective subsidy; indirect beneficiaries with trapped exit from hazard exposure.
 *   - Disaster researchers (observer/analytical): external analysts who evaluate whether drill regimes produce adaptive capacity or ritual compliance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.28).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.35).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission â Competence Reading").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster risk management/institutional memory/civil defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '3f526124-1f4c-4e4e-9b99-02e2fe0c37da').
narrative_ontology:cs_kernel_codification('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', formalized).
narrative_ontology:cs_authority_grounding('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', practice).
narrative_ontology:cs_interpretation_layer_present('3f526124-1f4c-4e4e-9b99-02e2fe0c37da').
narrative_ontology:cs_reading_relation('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', preparedness_transmission__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', foundational, operational_knowledge_requires_rehearsal).
narrative_ontology:cs_axiom_status(operational_knowledge_requires_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', operational_knowledge_requires_rehearsal, empirically_contingent).
narrative_ontology:cs_axiom('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', foundational, competence_validated_through_practice).
narrative_ontology:cs_axiom_status(competence_validated_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', competence_validated_through_practice, instrumental).
narrative_ontology:cs_reference_frame('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', live_practice_framework).
narrative_ontology:cs_drift_state('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3f526124-1f4c-4e4e-9b99-02e2fe0c37da', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, inspection_targets).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, protected_communities).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, operational_rehearsal_hypothesis).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, institutional_memory_embodiment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, mandates, and evaluates drill and inspection regimes. Authority derives from statutory civil defense mandate and professional emergency management standards. Exit is constrained by legal obligation to maintain preparedness; cannot abandon the regime without statutory change.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_authority, agenda_setter,
    institutional, generational, constrained, national).

% Participate in drills and inspections, exercising decision-making under simulated stress. Gain validated operational competence and adaptive capacity. Career path is embedded in civil defense; leaving means exiting the profession and the competence framework.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, response_personnel, beneficiary,
    moderate, biographical, constrained, regional).

% Agencies and facilities subject to readiness inspections. Receive structured feedback on preparedness gaps and resource priorities. Compliance is mandatory but yields validated operational status and allocation credibility within the civil defense system.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inspection_targets, beneficiary,
    organized, biographical, constrained, regional).

% Populations in hazard zones who depend on competent emergency response. Do not directly participate in drills but are the ultimate beneficiaries of maintained response capability. Geographic and economic constraints make relocation difficult; they cannot opt out of civil defense coverage.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, protected_communities, beneficiary,
    powerless, generational, trapped, local).

% Study preparedness effectiveness across jurisdictions. Evaluate whether drill regimes produce adaptive capacity or ritual compliance. Their findings inform policy but do not determine institutional practice.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, disaster_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational competence across personnel turnover and institutional memory decay by requiring regular physical rehearsal of emergency procedures under variable scenarios, preventing skill atrophy and inter-agency coordination failure.
% TRANSFER_FUNCTION: Moves time, attention, and material resources from routine operations into validated readiness exercises; moves competence from experienced personnel to new generations through supervised practice and feedback.
% ABSENT_VOICES: Communities that have never experienced disaster and therefore discount preparedness value; personnel who have left the profession and could attest to skill atrophy outside the drill regime; alternative preparedness models such as community-led decentralized response that are not formally recognized in the institutional framework.
% DISAPPEARANCE_RATIONALE: If the drill and inspection regime vanished overnight, operational competence would decay within one personnel turnover cycle, response coordination would fragment, and institutional memory would shift from embodied practice to inert documentation. The civil defense system would rearrange around ad hoc capacity or experience catastrophic failure in the next major event.
% FOUNDING_PROBLEM: Disaster response competence cannot be maintained by documentation alone; without regular physical rehearsal, decision-making under uncertainty atrophies and inter-agency coordination fails during crisis.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster sociology research and military operations research attest that skill degradation occurs without rehearsal; however, these researchers sit outside the benefiting civil defense bureaucracy and note that drill design quality, not mere frequency, determines whether the founding problem is actually addressed.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored descriptively for a well-functioning preparedness regime. Extractiveness is low-moderate (0.28) because the regime consumes real resources but returns validated competence. Suppression is moderate (0.35) because participation is institutionally mandated and alternatives such as decentralized or ad hoc preparedness are professionally marginalized. Theater ratio is low (0.22) because the competence reading asserts most drill activity is functional rehearsal rather than performance. Accessibility collapse is moderate-high (0.62) because once the premise that competence requires rehearsal is accepted, alternatives like documentation-only training conceptually collapse. Resistance is low (0.18) because participants in a functioning system perceive net benefit. Measurements track a flat-to-slightly-declining trajectory consistent with institutional optimization over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary institutional maintenance with symmetric costs and benefits. Response personnel experience it as professional development with biographical costs but net capability gain. Protected communities experience it as diffuse subsidy â protective capacity they do not directly pay for. The engine computes each seat's effective extraction from these structural positions; no seat is targeted for asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense authority sits near symmetric (d approx 0.5) â it both pays the cost of administering drills and benefits from validated institutional function. Response personnel and inspection targets are beneficiaries (low d) because they gain transferable competence and operational status. Protected communities are strong beneficiaries (very low d) receiving protective subsidy. Disaster researchers are analytical (analytical exit, neutral d). No agent is structurally targeted for extraction; the regime is symmetrically coordinative.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 founding problem â that disaster response competence atrophies without physical rehearsal â is corroborated as live by independent operations research and disaster sociology. The problem the constraint was built to solve still exists, which prevents mandatrophy mislabeling. If the founding problem were dead and the regime persisted, it would compute as piton or snare; here the live founding problem supports the rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ritual_ambiguity,
    'Is the observed drill performance genuinely adaptive competence, or theatrical performance that mimics competence under routine scenarios but would fail under novel stress?',
    'Introduce unannounced scenario variations and measure improvisation quality; compare drill outcomes against real-world event performance data.',
    'If performance is theatrical under novel stress, extractiveness and theater_ratio rise substantially, shifting classification toward tangled_rope or piton; if genuinely adaptive, the rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ritual_ambiguity, empirical, 'Empirical test of whether drill competence is adaptive or performative').

omega_variable(
    generational_transfer_efficacy,
    'Does each generation of personnel actually gain competence through drill participation, or does the regime merely filter for pre-existing aptitude while creating an appearance of training efficacy?',
    'Longitudinal tracking of individual responder performance from entry through retirement, controlling for prior experience, comparing drilled versus non-drilled career paths.',
    'If drills merely filter rather than build, the coordination function is hollow and the constraint extracts participation-time without returning competence, shifting toward snare or husk classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transfer_efficacy, empirical, 'Whether drills build competence or merely select for it').

omega_variable(
    kernel_reading_contest,
    'This constraint is the competence reading of the preparedness_transmission kernel. The husk reading sees the same practices as hollowed ritual, and the hybrid reading sees stratified decay. Which structural features distinguish live competence from performative maintenance?',
    'Cross-reading comparison of scenario improvisation rates, inspector cognitive load during evaluations, and post-drill knowledge-retention tests across jurisdictions.',
    'The structural elements that distinguish this reading (improvisation, novel signature recognition, generational re-validation) are the empirical boundary between rope and piton/snare classifications for this kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Conceptual boundary between competence and ritual readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(prep_tr_t4, preparedness_transmission__competence_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__competence_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(prep_tr_t12, preparedness_transmission__competence_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__competence_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__competence_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t4, preparedness_transmission__competence_reading, base_extractiveness, 4, 0.29).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__competence_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(prep_be_t12, preparedness_transmission__competence_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__competence_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__competence_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel decomposes into three structurally distinct constraints (competence_reading, husk_reading, hybrid_reading) because the same institutional practices (drills and inspections) admit divergent epsilon values depending on whether operational knowledge is live, hollowed, or stratified. This story links to its sibling readings as members of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
