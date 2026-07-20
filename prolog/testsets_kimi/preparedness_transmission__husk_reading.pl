% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Preparedness Transmission â Husk Reading (Ritual Maintenance)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   A civil defense system continues to perform drills and inspections
 *   inherited from an era of live operational competence. Over decades, the
 *   exercises have become memorial rituals: organizational memory of the form
 *   persists, but the adaptive judgment required for novel flood scenarios
 *   has hollowed out. Inspection routines detect only pre-specified failure
 *   modes, and high compliance masks low functional capacity. This constraint
 *   is the husk reading of the preparedness_transmission kernel, decomposed
 *   from the competence and hybrid readings.
 *
 * KEY AGENTS:
 *   - civil_defense_administration: Agenda-setter (institutional/constrained) â administers the ritual, could reform but doesn't, bears no concentrated cost.
 *   - emergency_responders: Primary payer (moderate/identity_locked) â perform hollow drills, bear deskilling and moral injury.
 *   - at_risk_populations: Secondary payer (powerless/trapped) â fund the system and bear false security and unmitigated flood risk.
 *   - disaster_risk_researchers: Analytical observer (analytical/analytical) â document the competence gap but lack standing to force revision.
 *   - grassroots_preparedness_groups: Excluded voice (powerless/constrained) â would build adaptive capacity but are locked out of state-certified channels.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.42).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.35).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Transmission â Husk Reading (Ritual Maintenance)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, 'a2cad713-f5d4-491b-a62d-668aef6c8a16').
narrative_ontology:cs_kernel_codification('a2cad713-f5d4-491b-a62d-668aef6c8a16', implicit).
narrative_ontology:cs_authority_grounding('a2cad713-f5d4-491b-a62d-668aef6c8a16', practice).
narrative_ontology:cs_interpretation_layer_present('a2cad713-f5d4-491b-a62d-668aef6c8a16').
narrative_ontology:cs_reading_relation('a2cad713-f5d4-491b-a62d-668aef6c8a16', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2cad713-f5d4-491b-a62d-668aef6c8a16', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a2cad713-f5d4-491b-a62d-668aef6c8a16', foundational, ritual_performance_fulfills_preparedness_obligation).
narrative_ontology:cs_axiom_status(ritual_performance_fulfills_preparedness_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a2cad713-f5d4-491b-a62d-668aef6c8a16', ritual_performance_fulfills_preparedness_obligation, conventional).
narrative_ontology:cs_axiom('a2cad713-f5d4-491b-a62d-668aef6c8a16', foundational, operational_competence_unmeasurable_by_protocol_audit).
narrative_ontology:cs_axiom_status(operational_competence_unmeasurable_by_protocol_audit, holdable).
narrative_ontology:cs_axiom_grounding('a2cad713-f5d4-491b-a62d-668aef6c8a16', operational_competence_unmeasurable_by_protocol_audit, conventional).
narrative_ontology:cs_reference_frame('a2cad713-f5d4-491b-a62d-668aef6c8a16', live_competence_transmission).
narrative_ontology:cs_drift_state('a2cad713-f5d4-491b-a62d-668aef6c8a16', contemporary_ritual_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2cad713-f5d4-491b-a62d-668aef6c8a16', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, emergency_responders).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, at_risk_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schedules and certifies drills and inspections according to legacy protocols inherited from earlier eras of operational competence. Retains budget lines, political visibility, and inter-agency standing by demonstrating activity. Could reform training toward adaptive scenarios but faces no external pressure to do so and internal culture treats protocol completion as success.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_administration, agenda_setter,
    institutional, generational, constrained, national).

% Perform repeated drills that test only pre-specified failure modes. Experience progressive deskilling and moral injury as field-relevant judgment is replaced by scripted compliance. Professional identity is fused with uniformed service and ceremonial readiness, making public acknowledgment of hollowness feel like self-betrayal.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, emergency_responders, payer,
    moderate, biographical, identity_locked, regional).

% Fund the apparatus through taxation and rely on public readiness signaling. Do not have access to independent assessment of adaptive capacity. Bear the catastrophic downside when a novel flood scenario exceeds the ritualized response repertoire.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, at_risk_populations, payer,
    powerless, biographical, trapped, local).

% Document the widening gap between protocol compliance and exercised competence in after-action reviews and peer-reviewed studies. Findings circulate in academic and policy channels but lack enforcement leverage or institutional standing to compel reform.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, disaster_risk_researchers, observer,
    analytical, civilizational, analytical, global).

% Would organize neighborhood-level adaptive training and mutual-aid flood response. Barred from official planning channels because legitimacy is reserved for state-certified protocol performance; their knowledge is treated as informal and non-authoritative.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, grassroots_preparedness_groups, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve operational preparedness across long inter-event periods and personnel turnover by regularly exercising response protocols so that competence does not decay.
% TRANSFER_FUNCTION: Moves tax revenue and personnel time into scheduled drills and inspections; moves public trust and risk-awareness into state-certified readiness signals that no longer correlate with exercised competence under novel conditions.
% ABSENT_VOICES: Grassroots preparedness groups are excluded from official channels; frontline responders who know protocols are inadequate are muted by hierarchical discipline and identity fusion; disaster risk researchers observing the competence gap lack institutional authority to force revision.
% DISAPPEARANCE_RATIONALE: If the ritualized drills and inspections ceased, institutional time and budget would be forced into alternative allocation; the false security signal would end, potentially opening space for genuine adaptive capacity building; the administrative apparatus would shrink or reorient away from ceremonial compliance.
% FOUNDING_PROBLEM: Catastrophic flood events revealed that uncoordinated, untrained response leads to mass casualties; institutional memory of effective response needed to be preserved across long quiet periods and changing personnel.
% FOUNDING_PROBLEM_CORROBORATION: Disaster risk researchers and independent post-event after-action reports attest that protocol-compliant units repeatedly fail in novel scenarios; the civil defense administration itself cites drill completion rates rather than outcome-based competence, corroborating that the founding problem is no longer served.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).
:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the ritual consumes real resourcesâpersonnel time, tax revenue, and public trustâwithout returning adaptive capacity. Theater_ratio is high (0.78) because the visible performance of readiness is the primary remaining output; the ratio of performative to functional activity dominates. Suppression is moderate-low (0.35): alternatives are not actively crushed but are crowded out by institutional monopoly and identity fusion. Accessibility_collapse is moderate (0.55): community-based alternatives are visible but institutionally inaccessible because legitimacy is tied to formal protocol. Resistance is low (0.25) because costs are diffuse and the ritual is culturally inscribed as responsible governance. The measurement series runs on a single shared time grid, showing monotonic drift from genuine coordination toward piton.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (civil defense administration) experiences the constraint as fulfilling a legitimate mandate and maintaining institutional continuity. The payer seats (emergency responders and at-risk populations) experience it as a performative drain that obscures real vulnerability. The researcher seat sees the structural gap but has no leverage to close it. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency responders and at-risk populations are declared victims (bear costs, high directionality toward target). The civil defense administration is not declared a beneficiary because the piton structure lacks concentrated capture; its structural position is inertia-bound rather than extractive. No directionality overrides are needed because the absence of beneficiaries and presence of victims correctly orients the derivation chain: the administration receives a canonical fallback near symmetric, while the victims are pulled toward the full-target end by their trapped and identity-locked exits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving live competence across inter-event periodsâis dead; the drills no longer exercise adaptive capacity. The R5 genealogy flags the constraint as a zombie mandate (founding_problem_status: dead, disappearance_verdict: world_rearranges). The piton classification captures that the arrangement persists not because it solves the founding problem (a scaffold would require a sunset clause and transition logic) nor because it actively extracts (a snare would require a concentrated beneficiary), but because institutional inertia and identity lock make dissolution more costly than maintenance for the agenda-setter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_of_operational_decay,
    'Can the hollowed operational knowledge be recovered within the existing institutional framework, or has ritualization created path-dependent lock-in that requires institutional replacement?',
    'Comparative study of civil defense systems that successfully reformed after ritualization versus those that required dissolution and rebuilding.',
    'If reversible, the constraint is a degraded scaffold rather than a piton; if irreversible, it is a permanent structural trap requiring external intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_operational_decay, empirical, 'Whether ritualization is reversible or path-locked.').

omega_variable(
    beneficiary_concentration_ambiguity,
    'Does the civil defense administration capture concentrated non-budgetary benefits (political visibility, career preservation, institutional prestige) that would reclassify the constraint from piton to snare?',
    'Ethnographic and administrative analysis of incentive structures within the agency; tracking whether leadership actively resists reform to protect personal or institutional rents.',
    'If concentrated capture exists, the constraint is a snare masquerading as inertial ritual; if absent, the inertial piton classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_concentration_ambiguity, conceptual, 'Whether diffuse inertia masks concentrated extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__husk_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.78).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__husk_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the husk reading of the preparedness_transmission kernel, decomposed per the Îµ-invariance principle from the competence and hybrid readings. Each reading carries a distinct Îµ and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
