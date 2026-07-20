% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the competence reading of the
 *   preparedness_commitment kernel. Under this reading, institutional
 *   preparedness routines are live exercised knowledge that maintains
 *   operational capacity across generational turnover. The expected
 *   structural delta is high adaptive capacity, drills that test real
 *   decision-making, and D5 generational breaks avoided or contained. Sibling
 *   readings treat the same observable routines differently: husk_reading
 *   views them as performative memorial lacking competence, while
 *   hybrid_reading decomposes the system into functional and memorial layers.
 *   This story authors ONLY the competence reading as a clean, Îµ-invariant
 *   constraint per Rule 1; the low extractiveness and theater metrics reflect
 *   the judgment that the routines constitute genuine coordination rather
 *   than extraction.
 *
 * KEY AGENTS:
 *   - current_operations_personnel: Primary beneficiary/payer (moderate/constrained) â bears the time and cognitive cost of live exercises, gains operational competence and adaptive capacity.
 *   - successor_generations: Secondary beneficiary (powerless/constrained) â inherits maintained operational capacity and benefits from the absence of D5 generational breaks.
 *   - affected_communities: Diffuse beneficiary (powerless/constrained) â relies on institutional response capacity maintained across turnover.
 *   - institutional_leadership: Agenda setter (institutional/constrained) â allocates resources and mandates exercises to maintain legitimacy and readiness across political cycles.
 *   - training_designers: Agenda setter (moderate/mobile) â designs scenarios that translate operational experience into rehearsed adaptive decisions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.2).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.22).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, 'f0748be0-48f6-42cd-acd0-d9dc75fc2d81').
narrative_ontology:cs_kernel_codification('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', implicit).
narrative_ontology:cs_authority_grounding('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', practice).
narrative_ontology:cs_interpretation_layer_present('f0748be0-48f6-42cd-acd0-d9dc75fc2d81').
narrative_ontology:cs_reading_relation('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', foundational, competence_through_exercise).
narrative_ontology:cs_axiom_status(competence_through_exercise, holdable).
narrative_ontology:cs_axiom_grounding('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', competence_through_exercise, empirically_contingent).
narrative_ontology:cs_axiom('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', foundational, generational_absorption_via_rehearsal).
narrative_ontology:cs_axiom_status(generational_absorption_via_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', generational_absorption_via_rehearsal, empirically_contingent).
narrative_ontology:cs_reference_frame('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', live_exercised_competence).
narrative_ontology:cs_drift_state('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', generational_turnover_present, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f0748be0-48f6-42cd-acd0-d9dc75fc2d81', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, current_operations_personnel).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, successor_generations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, affected_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, current_operations_personnel).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, generational_knowledge_transfer).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, tacit_knowledge_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in recurring live exercises and drills that rehearse decision-making under stress. They bear the time and cognitive load of training but gain operational competence and adaptive capacity that written protocols cannot provide.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, current_operations_personnel, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, current_operations_personnel, payer).

% Inherit the operational capacity maintained by predecessors through live exercised knowledge. They benefit from the absence of D5 generational breaks and enter an institution where tacit knowledge has been transferred through practice rather than degraded into documentation.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, successor_generations, beneficiary,
    powerless, generational, constrained, national).

% Receive the benefit of institutional response capacity when disaster occurs. Their safety depends on the operational competence of personnel they never meet, maintained across generational turnover through exercised routines.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, affected_communities, beneficiary,
    powerless, biographical, constrained, local).

% Allocates resources to training programs and mandates recurring exercises. They depend on the continued functioning of the routines to justify budget and maintain institutional legitimacy across political cycles.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Design and facilitate live exercises that test decision-making under uncertainty. They translate operational experience into rehearsed scenarios, ensuring the routines remain adaptive to changing threats.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, training_designers, agenda_setter,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational competence across generational turnover so that decision-making capacity under stress does not decay when experienced personnel retire or rotate.
% TRANSFER_FUNCTION: Moves practiced knowledge, validated reflexes, and adaptive decision-making protocols from experienced personnel to successor generations through repeated live exercises and scenario rehearsals.
% ABSENT_VOICES: Communities that have never experienced a major disaster and therefore undervalue preparedness spending; personnel who left institutions where preparedness decayed into husk-state and could contrast the two regimes; external auditors without operational experience who cannot distinguish exercised competence from ceremonial drill.
% DISAPPEARANCE_RATIONALE: Without live exercised knowledge, institutional memory would thin to documentation-only within one to two generational turnovers. Decision-making under uncertainty would degrade into brittle rule-following, and the institutional capacity to adapt to novel crises would rearrange toward procedural compliance or collapse.
% FOUNDING_PROBLEM: Generational turnover in operational institutions inherently erodes crisis-response competence because written records cannot encode tacit knowledge, and effective decision-making under uncertainty requires validated reflexes that decay without regular rehearsal.
% FOUNDING_PROBLEM_CORROBORATION: Disaster researchers and organizational sociologists outside the training bureaucracy attest to generational knowledge loss in high-consequence domains. Independent after-action reviews confirm that exercised competence produces measurably different outcomes than documentation-based response. Successor personnel corroborate that live drills convey situational judgment that manuals cannot.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.20 because the constraint moves resources toward training and rehearsal, but these costs are symmetrically borne by participants who gain operational competence in return; there is no asymmetric extraction. Suppression is low (0.22) because alternatives (relying solely on documentation, reducing drill frequency) are not actively suppressedâthey are understood to be inferior based on operational experience, not coerced. Theater_ratio is low (0.15) because the drills are functional: they test real decision-making and adapt to changing threats. Accessibility_collapse is moderately high (0.62) because once the tacit-knowledge decay problem is understood, the alternative of documentation-only preparedness collapses as a viable strategy. Resistance is moderate-low (0.30) because live exercises carry natural burden but participants generally recognize their value.
 *
 * PERSPECTIVAL GAP:
 *   Current operations personnel experience the constraint as recurring effort and time cost in the present, while successor generations and affected communities receive the benefit of maintained competence in future crises. The engine will compute different per-seat weightings based on this temporal asymmetry: the payer-like burden sits on the current cohort, while the beneficiary position sits on future cohorts. However, because the current cohort also gains competence that reduces their own future failure risk, the net directionality remains on the beneficiary side rather than the target side.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared parties are beneficiaries of the coordination function. Current operations personnel are listed in beneficiaries because they gain adaptive capacity that protects them operationally; their constrained exit and moderate power would otherwise suggest target-like position, but the structural beneficiary declaration overrides this toward subsidy. Successor generations and affected communities are pure beneficiaries with no countervailing cost. Agenda setters (institutional leadership, training designers) are not beneficiaries in the rent-collection sense; they administer the coordination and are structurally positioned to maintain it. No victim group is declared because the competence reading finds no asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy because the founding problemâgenerational erosion of tacit crisis competenceâremains live. The arrangement would not persist purely by inertia; if the exercises ceased to test real decision-making and became ceremonial (the husk reading), the coordination function would die and the constraint would drift toward piton or snare. The authored metrics (low theater, low extraction, live founding problem) align with a genuine rope rather than a captured or atrophied structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the competence reading of kernel preparedness_commitment; siblings husk_reading and hybrid_reading assign different structural descriptions to the same routines. Is the observed institutional behavior live exercised knowledge, performative memorial, or a hybrid?',
    'Independent operational audit of exercised decision quality under simulated and actual stress; comparison of drill outcomes to post-crisis after-action reviews.',
    'If the husk reading is empirically correct for this institution, theater_ratio and extractiveness are substantially higher than authored, and the constraint reclassifies toward piton or snare. If hybrid, the constraint decomposes into linked sub-constraints with different Îµ values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity between competence, husk, and hybrid readings of the preparedness commitment kernel.').

omega_variable(
    generational_transfer_measurement,
    'Can generational knowledge transfer be measured independently of institutional self-reporting?',
    'Longitudinal tracking of error rates and decision latency across cohorts; blind evaluation of exercised responses by external operational analysts.',
    'Would validate or invalidate the foundational empirical premise that live exercise absorbs generational turnover, affecting the rope classification stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transfer_measurement, empirical, 'Whether generational competence transfer is measurable and real.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.21).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the preparedness_commitment family. The competence reading (this file) isolates the functional coordination component with low extraction. The husk reading isolates the performative memorial component with high theater and extraction. The hybrid reading would decompose the kernel into linked functional and memorial subsystems. They share a regulatory domain (institutional preparedness) but have different Îµ values and stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
