% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Drills and Inspections as Live-Exercised Preparedness Competence
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness_transmission kernel: drills and inspections function as
 *   genuinely live-exercised knowledge, where each generation of participants
 *   and inspectors re-validates real operational capability through practice
 *   against varied, realistic failure scenarios. Inspectors detect novel
 *   failure signatures rather than checking boxes against a fixed rubric;
 *   drill participants demonstrate improvisation under scenario variation
 *   rather than rote script-following. This is a low-extraction,
 *   coordination-dominant reading — the friction cost of drilling is real but
 *   the capability it produces is genuine and would visibly rearrange the
 *   world if withdrawn. The sibling husk_reading and hybrid_reading are
 *   separate constraints, not alternative measurements of this one: they
 *   describe different structural states of the same kernel (ritualized
 *   hollowing vs. stratified decay) and carry their own ε values and
 *   stakeholder sets.
 *
 * KEY AGENTS:
 *   - emergency_response_agencies: agenda_setter (institutional/constrained) — designs and runs the exercised loop
 *   - field_inspectors: agenda_setter/beneficiary (moderate/constrained) — personally exposed to novel failure detection
 *   - resident_populations: beneficiary/payer (powerless/constrained) — bear drill friction, gain executable capability
 *   - infrastructure_operators: beneficiary (organized/constrained) — validated engineering competence protects against liability
 *   - next_generation_responders: beneficiary (moderate/constrained) — inherit tacit judgment only through live practice
 *   - independent_auditors: observer (analytical/analytical) — assess whether competence is real
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.22).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.28).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Drills and Inspections as Live-Exercised Preparedness Competence").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '26bd48d0-3ede-4c8f-8389-cfee158abb57').
narrative_ontology:cs_kernel_codification('26bd48d0-3ede-4c8f-8389-cfee158abb57', formalized).
narrative_ontology:cs_authority_grounding('26bd48d0-3ede-4c8f-8389-cfee158abb57', practice).
narrative_ontology:cs_interpretation_layer_present('26bd48d0-3ede-4c8f-8389-cfee158abb57').
narrative_ontology:cs_reading_relation('26bd48d0-3ede-4c8f-8389-cfee158abb57', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('26bd48d0-3ede-4c8f-8389-cfee158abb57', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('26bd48d0-3ede-4c8f-8389-cfee158abb57', foundational, practice_generates_verifiable_capability).
narrative_ontology:cs_axiom_status(practice_generates_verifiable_capability, holdable).
narrative_ontology:cs_axiom_grounding('26bd48d0-3ede-4c8f-8389-cfee158abb57', practice_generates_verifiable_capability, empirically_contingent).
narrative_ontology:cs_axiom('26bd48d0-3ede-4c8f-8389-cfee158abb57', secondary, scenario_variation_produces_genuine_adaptive_transfer).
narrative_ontology:cs_axiom_status(scenario_variation_produces_genuine_adaptive_transfer, holdable).
narrative_ontology:cs_axiom_grounding('26bd48d0-3ede-4c8f-8389-cfee158abb57', scenario_variation_produces_genuine_adaptive_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('26bd48d0-3ede-4c8f-8389-cfee158abb57', actively_exercised_capability_standard).
narrative_ontology:cs_drift_state('26bd48d0-3ede-4c8f-8389-cfee158abb57', contemporary_multi_generation_drill_cycle, gap(stable, minor, true)).
narrative_ontology:cs_created_at('26bd48d0-3ede-4c8f-8389-cfee158abb57', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, resident_populations).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, infrastructure_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, field_inspectors).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, next_generation_responders).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, resident_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and runs the drill and inspection cycle, sets scenario variation, and evaluates performance against evolving failure modes. Bears the reputational and legal cost if a real event exposes drill inadequacy, so it has strong incentive to keep exercises genuinely diagnostic rather than ceremonial.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_response_agencies, agenda_setter,
    institutional, generational, constrained, regional).

% Conduct the physical inspections and drill evaluations, personally exposed to novel failure signatures during each cycle. Their professional standing depends on genuinely detecting problems, which sustains their skill and vigilance across career-length horizons.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, field_inspectors, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, field_inspectors, beneficiary).

% Participate in evacuation and shelter drills, bear the friction and disruption cost of practice, but gain the direct benefit of a population and set of first responders who can actually execute the plan when a real disaster strikes. Cannot opt out of local drill schedules without leaving the jurisdiction.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, resident_populations, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, resident_populations, payer).

% Subject their physical plant (dams, grids, shelters) to recurring inspection and stress-test drills; the inspections validate that engineering competence has not silently degraded, which protects them from catastrophic failure liability and keeps their systems certified for operation.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, infrastructure_operators, beneficiary,
    organized, generational, constrained, regional).

% New recruits and junior staff who inherit tacit knowledge only through live practice under realistic scenario variation; without the exercised loop they would have no path to acquire the judgment that veterans built through prior real events.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, next_generation_responders, beneficiary,
    moderate, generational, constrained, regional).

% Periodically review after-action reports and drill outcome data to assess whether the exercised competence is real or has quietly become ritualized; they compare drill scenario complexity and inspector detection rates against actual incident outcomes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, independent_auditors, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Recurring drills and inspections re-validate that response capability actually exists in the people and infrastructure that would need to act during a disaster — knowledge that decays without periodic live exercise is refreshed and stress-tested against realistic, varying failure scenarios.
% TRANSFER_FUNCTION: Moves attention, disruption cost, and practice-time from participants (residents, operators, junior staff) into a shared, continuously re-verified stock of executable capability that the whole jurisdiction draws on during an actual event.
% ABSENT_VOICES: Populations in jurisdictions with no drill program at all have no comparative voice in whether the exercised model is worth its friction cost; they are not excluded from THIS reading's own drills, but the counterfactual of chronic under-drilling is not represented in the record this reading examines.
% DISAPPEARANCE_RATIONALE: If live drills and inspections stopped, the tacit, perishable skills of coordinated evacuation, structural stress response, and inter-agency handoff would degrade within one staff generation; infrastructure certification would become paper-only, and the first real event would reveal a capability gap that the exercised loop currently forecloses.
% FOUNDING_PROBLEM: Disasters are rare enough that the skills needed to respond to them are never naturally rehearsed by the people who must use them, so without deliberate periodic practice, institutional and civic capability silently atrophies between events.
% FOUNDING_PROBLEM_CORROBORATION: Independent auditors and post-incident review boards outside the drilling agencies themselves attest that jurisdictions with active, scenario-varied drill programs show measurably faster and more coordinated real-event response than comparable jurisdictions with dormant or paper-only programs; this corroboration comes from bodies with no stake in continuing the drill budget.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is low (0.22) because the drill and inspection cycle transfers effort into a capability stock that participants themselves draw on — there is no concentrated party extracting rent from the arrangement under this reading. Suppression is moderate-low (0.28) because participation in official drills is mandatory (regulatory or contractual), but this reflects genuine coordination-problem enforcement (free-riding on shared safety infrastructure) rather than extraction. Theater ratio stays low and only mildly rising (0.10 to 0.15) across the interval, consistent with the competence reading's core claim that the exercises remain substantively diagnostic rather than becoming performative — a small upward drift is authored honestly because even competence-reading institutions accumulate some ceremonial residue over decades, but it stays well below the husk-reading threshold.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency response agencies and field inspectors sit near the agenda-setting end but are not simple beneficiaries collecting rent — their professional and institutional survival depends on the exercised knowledge remaining real, so their directionality is closer to symmetric-with-skin-in-the-game than to pure extraction. Resident populations and infrastructure operators are beneficiaries with a secondary payer role: they bear the friction cost of drilling but receive the larger benefit of validated capability, keeping their directionality damped toward the beneficiary end rather than the target end. No agent is authored as a pure victim under this reading, consistent with the absence of a victims declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence reading is precisely the case where mandatrophy does NOT apply: the founding problem (perishable skill atrophy between rare disasters) remains live, and independent auditors outside the benefiting agencies corroborate that the exercised loop still produces measurably faster real-event coordination. This blocks the arrangement from being mislabeled as pure extraction or ritual — the six_questions disappearance_verdict of world_rearranges is earned by evidence, not asserted by the drilling agencies themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_discriminator,
    'Is the observed drill and inspection performance genuinely diagnostic (competence_reading) or has it hollowed into ritual performance that merely resembles diagnostic activity (husk_reading)?',
    'Compare drill scenario novelty and complexity over time against actual incident outcomes and after-action gap analysis; a competence reading predicts inspectors flag genuinely novel failure signatures at a stable or rising rate, while a husk reading predicts detection rates converge on a fixed rubric regardless of scenario variation.',
    'If the discriminator resolves toward husk_reading, this story''s low theater_ratio and low extractiveness values would be structurally wrong for the actual arrangement being measured — the correct classification would shift to a degraded/inertial reading (piton-adjacent) rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_discriminator, empirical, 'Whether the exercised loop remains genuinely diagnostic or has become ritualized without the story''s own metrics detecting it.').

omega_variable(
    stratification_boundary,
    'Is the competence uniform across physical-infrastructure and civilian-coordination domains, or does it stratify as the hybrid_reading claims — high engineering competence coexisting with decayed civic coordination knowledge?',
    'Disaggregate drill outcome data by domain: infrastructure stress-test pass rates versus civilian evacuation coordination timing and error rates, tracked separately over multiple drill cycles.',
    'If stratification is confirmed, this single-constraint competence_reading over-claims uniform adaptive capacity; the correct authoring move would be to split resident_populations and next_generation_responders coordination competence into a separate, lower-ε constraint aligned with hybrid_reading, leaving infrastructure_operators competence under this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary, conceptual, 'Whether the kernel''s competence is domain-uniform (as this reading assumes) or stratified between physical and civic knowledge.').

omega_variable(
    auditor_independence_durability,
    'Will independent auditors remain structurally independent of the drilling agencies over multi-decade horizons, or will auditor capture eventually compromise the corroboration this reading relies on?',
    'Track auditor funding sources, appointment mechanisms, and career paths for evidence of eventual absorption into the agencies they review.',
    'If auditor independence erodes, the founding_problem_corroboration this reading depends on becomes self-referential (agencies corroborating themselves), which would undermine the evidentiary basis for classifying this as the live competence reading rather than a husk in early stages.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(auditor_independence_durability, empirical, 'Long-horizon risk that the external corroboration source for this reading''s competence claim loses independence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__competence_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__competence_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__competence_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__competence_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__competence_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__competence_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__competence_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__competence_reading, base_extractiveness, 24, 0.21).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__competence_reading, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__competence_reading, base_extractiveness, 40, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.26).
narrative_ontology:measurement(prep_su_t8, preparedness_transmission__competence_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(prep_su_t16, preparedness_transmission__competence_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__competence_reading, suppression_requirement, 24, 0.27).
narrative_ontology:measurement(prep_su_t32, preparedness_transmission__competence_reading, suppression_requirement, 32, 0.28).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__competence_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_transmission kernel. competence_reading (this file) claims genuinely live, adaptively current exercised knowledge across both physical infrastructure and civilian coordination domains — low extraction, rope-flavored. husk_reading claims the same drill and inspection apparatus persists as memorial ritual with hollowed operational content — expect a piton-flavored classification with elevated theater_ratio. hybrid_reading claims stratification: engineering competence remains high while civilian coordination knowledge has decayed — expect a mixed or tangled_rope-flavored classification with domain-differentiated stakeholders. Each reading authors its own ε and stakeholder set per the ε-invariance principle; they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
