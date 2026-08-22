% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Disaster Drills and Inspections as Live-Validated Competence
 *   domain: disaster_risk_management/institutional_memory
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness_transmission kernel: drills and inspections in disaster risk
 *   management function as live-exercised knowledge, where each cycle of
 *   practice actually re-validates capability rather than merely performing
 *   it. On this reading, scenario variation is deliberately engineered to
 *   surface novel failure signatures, inspectors develop genuine
 *   pattern-recognition transferable to unscripted conditions, and drill
 *   participants build improvisational capacity rather than rote
 *   script-following. This is a low-extraction, low-suppression reading: the
 *   coordination function (skill retention across a
 *   low-frequency-high-consequence hazard domain) dominates, and the costs
 *   borne by response agencies, infrastructure operators, and the drill
 *   workforce are proportionate to a genuine, functioning coordination good.
 *   The sibling readings (husk_reading, hybrid_reading) describe the same
 *   institutional apparatus but claim the operational content has hollowed
 *   out either wholesale or selectively; those are separate constraints with
 *   their own ε and stakeholder structures, not alternative measurements of
 *   this one.
 *
 * KEY AGENTS:
 *   - civil_defense_agency: agenda_setter (institutional/analytical) — designs scenario-varied drills and certifies capability from live performance
 *   - residents_in_hazard_zones: beneficiary (moderate/constrained) — depend on the certified capacity actually existing when disaster strikes
 *   - emergency_response_agencies: beneficiary/payer (organized/constrained) — stress-tested and genuinely improved by the exercises, at recurring operational cost
 *   - infrastructure_operators: beneficiary/payer (powerful/constrained) — inspected under conditions that surface real degradation before failure
 *   - drill_participant_workforce: payer/beneficiary (moderate/constrained) — bears the physical and career cost while building the retained skill
 *   - independent_capability_auditors: observer (institutional/analytical) — corroborates or contests the competence claim from outside the certifying agency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.22).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Disaster Drills and Inspections as Live-Validated Competence").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '6b4886bb-3b33-4a37-8e94-b46768ed8ec7').
narrative_ontology:cs_kernel_codification('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', implicit).
narrative_ontology:cs_authority_grounding('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', practice).
narrative_ontology:cs_interpretation_layer_present('6b4886bb-3b33-4a37-8e94-b46768ed8ec7').
narrative_ontology:cs_reading_relation('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', foundational, scenario_variation_produces_generalizable_skill).
narrative_ontology:cs_axiom_status(scenario_variation_produces_generalizable_skill, holdable).
narrative_ontology:cs_axiom_grounding('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', scenario_variation_produces_generalizable_skill, empirically_contingent).
narrative_ontology:cs_axiom('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', secondary, certification_reflects_live_performance_not_paper_compliance).
narrative_ontology:cs_axiom_status(certification_reflects_live_performance_not_paper_compliance, holdable).
narrative_ontology:cs_axiom_grounding('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', certification_reflects_live_performance_not_paper_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', live_exercised_competence_standard).
narrative_ontology:cs_drift_state('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6b4886bb-3b33-4a37-8e94-b46768ed8ec7', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, residents_in_hazard_zones).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, infrastructure_operators).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, drill_participant_workforce).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, drill_participant_workforce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, infrastructure_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates the drill and inspection calendar, sets scenario variation to prevent rote memorization, and certifies capability based on live performance rather than paper compliance. Bears reputational and legal exposure if certified capacity fails during an actual event.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_agency, agenda_setter,
    institutional, generational, analytical, national).

% Depend on responders and infrastructure operators actually being able to execute evacuation, triage, and repair under real conditions. Cannot personally verify competence, only observe drill frequency and outcomes when disasters occur; exit means relocating away from the hazard zone, which most cannot readily do.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, residents_in_hazard_zones, beneficiary,
    moderate, biographical, constrained, regional).

% Participate in drills that genuinely stress-test their coordination and communication under scenario variation designed to surface novel failure modes. Gain real operational feedback and revised protocols, but bear the recurring cost in staff time, overtime, and equipment wear that drilling imposes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_response_agencies, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, emergency_response_agencies, payer).

% Submit dams, grids, and structural systems to inspection regimes that surface genuine degradation and failure signatures inspectors have been trained to recognize under live conditions. Costs are real (downtime, remediation spend) but the inspections catch problems before failure, which the operators themselves rely on to avoid catastrophic liability.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, infrastructure_operators, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, infrastructure_operators, payer).

% Firefighters, inspectors, and civil-defense conscripts who physically execute the repeated drills. Bear the fatigue, injury risk, and career cost of continuous re-certification, but are also the direct beneficiaries of the skill retention and improvisation capacity the exercises are designed to build in them.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, drill_participant_workforce, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, drill_participant_workforce, beneficiary).

% Represents the future failure modes that have not yet occurred and cannot speak for themselves in the drill-design process; the drill designers must anticipate them without direct testimony, which is the structural gap this reading claims is being closed by scenario variation rather than left open.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, novel_disaster_scenarios, excluded,
    analytical, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(preparedness_transmission__competence_reading, novel_disaster_scenarios).

% External reviewers (legislative oversight bodies, academic disaster researchers, insurance actuaries) who examine post-incident performance against drilled competence claims and can corroborate or contest whether the exercised knowledge actually transferred to real events.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, independent_capability_auditors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that emergency and infrastructure competence atrophies without practice and that written procedures alone do not transfer tacit skill; live, scenario-varied drills and inspections re-validate that capacity actually exists in the people and systems that would need to exercise it.
% TRANSFER_FUNCTION: Moves time, fatigue, and operating cost from response agencies, infrastructure operators, and the drill workforce into a public good — verified real-world capability — that residents in hazard zones and the agencies themselves draw on when an actual disaster occurs.
% ABSENT_VOICES: The disasters that have not yet happened cannot testify to whether the current scenario library covers their failure signature; this reading asserts inspectors and drill designers close that gap through generalizable skill and pattern recognition, but the claim is only as strong as the scenario design discipline behind it.
% DISAPPEARANCE_RATIONALE: If live drilling and inspection stopped, procedures would persist on paper but the tacit, improvisational capacity to execute them under real, non-scripted conditions would decay within a few years; certification would become paper-only, and the gap would only surface catastrophically at the next real event.
% FOUNDING_PROBLEM: Institutional and individual competence to respond to low-frequency, high-consequence disasters degrades between events; without periodic live re-validation, procedures written after one disaster are not reliably executable by the people and systems present for the next one.
% FOUNDING_PROBLEM_CORROBORATION: Independent capability auditors and post-incident academic reviews (outside the civil defense agency and the response agencies that benefit from favorable capability certification) attest that regions with high-fidelity, scenario-varied drilling show measurably better real-event outcomes than regions with rote or infrequent drilling — this is the evidentiary basis this reading rests on, distinct from the agency's own self-certification.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18 at interval end) because on this reading the coordination function is real and dominant: drills produce genuinely improved, generalizable capability rather than performing compliance. Suppression is low-moderate (0.22) because participation is mandated by professional and regulatory obligation but is not coercively extractive — the enforcement exists to guarantee the coordination good gets produced, not to extract rent. Theater ratio is kept low (0.15) and only mildly rising, reflecting that this reading's core empirical claim is precisely that performative drift has NOT set in; a rising theater ratio approaching or exceeding 0.5 would itself be evidence for the husk_reading, not this one. All three metrics move on one shared time grid.
 *
 * DIRECTIONALITY LOGIC:
 *   The civil defense agency sets the agenda but bears the accountability risk if certified capacity fails during a real event, keeping it close to a coordinating rather than purely extracting seat. Residents, response agencies, and infrastructure operators are net beneficiaries of a working competence-transmission system, even though response agencies and operators also pay real operating costs — hence their dual beneficiary/payer roles. The drill workforce is the seat that bears the most direct physical and career cost (fatigue, injury risk, repeated re-certification) while also being the direct locus of the skill this reading claims is genuinely being built; their dual role reflects that the same activity that costs them the most is also what benefits them most on this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (skill atrophy between low-frequency disaster events) remains structurally live regardless of reading — disasters are still low-frequency and high-consequence, and tacit skill still decays without practice. What distinguishes this reading from husk_reading is not whether the founding problem persists (it does, on all three readings) but whether the arrangement still actually solves it. Classifying this as a functioning rope, rather than defaulting to piton on the grounds that 'all long-running institutional rituals decay,' requires the specific empirical claim that scenario variation is real and that inspectors/participants demonstrate transfer to unscripted conditions — precisely the claim independent capability auditors are positioned to corroborate or refute from outside the certifying agency's own self-assessment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_discriminator,
    'What observable would distinguish genuine skill transfer (this reading) from hollowed-out ritual performance (husk_reading) when both produce the same drill calendar and the same certification paperwork?',
    'Post-incident performance audits comparing drilled predictions to actual event outcomes, blind scenario injection during drills (novel failure modes not in the training library), and independent (non-agency) observer scoring of improvisation quality under scenario variation.',
    'If post-incident audits and blind-injection results show drilled personnel and inspectors generalizing well to unscripted failure modes, this reading is corroborated. If audits instead show performance collapses outside the rehearsed script, the apparatus is better described by the husk_reading despite identical surface-level drill frequency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_discriminator, empirical, 'The discriminating evidence between the competence and husk readings of the same drill/inspection apparatus.').

omega_variable(
    stratification_boundary_location,
    'Even under this reading, is competence genuinely uniform across all domains (engineering, medical, civilian coordination, logistics), or does the hybrid_reading''s stratification claim apply to some sub-domains within what this story treats as a single apparatus?',
    'Domain-disaggregated post-incident review: compare structural/engineering inspection outcomes against civilian evacuation-coordination outcomes in the same events.',
    'If stratification is real, this reading may only hold for the physical-infrastructure sub-domain, and a separate, lower-ε story should be written for civilian coordination specifically — this omega flags where this single-constraint framing may itself need decomposition rather than remaining one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary_location, conceptual, 'Whether this reading''s uniform-competence claim should itself be decomposed along the hybrid_reading''s stratification line.').

omega_variable(
    self_certification_independence,
    'How independent is the corroboration this reading relies on — do independent capability auditors have genuine access and incentive to detect decay, or are they substantially dependent on the same agencies they audit?',
    'Examine auditor funding sources, appointment mechanisms, and historical instances where audit findings contradicted agency self-certification and were acted upon.',
    'If auditor independence is weak, the corroboration this reading cites is thinner than claimed, which would weaken confidence in competence_reading relative to husk_reading without changing the authored ε directly — this bears on how much weight the classification computed from this story''s data should carry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_certification_independence, empirical, 'The strength of the outside corroboration this reading depends on for its founding-problem-status claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prep_tr_t4, preparedness_transmission__competence_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__competence_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(prep_tr_t12, preparedness_transmission__competence_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__competence_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__competence_reading, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(prep_be_t4, preparedness_transmission__competence_reading, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__competence_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(prep_be_t12, preparedness_transmission__competence_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__competence_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__competence_reading, base_extractiveness, 24, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(prep_su_t4, preparedness_transmission__competence_reading, suppression_requirement, 4, 0.2).
narrative_ontology:measurement(prep_su_t8, preparedness_transmission__competence_reading, suppression_requirement, 8, 0.21).
narrative_ontology:measurement(prep_su_t12, preparedness_transmission__competence_reading, suppression_requirement, 12, 0.21).
narrative_ontology:measurement(prep_su_t16, preparedness_transmission__competence_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__competence_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__competence_reading, suppression_requirement, 24, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the preparedness_transmission kernel, decomposed per the ε-invariance principle: competence_reading (this file, ε≈0.18, rope), husk_reading (expected high theater_ratio and high ε, likely piton or snare depending on capture), and hybrid_reading (expected split ε across sub-domains, potentially requiring further decomposition per the stratification_boundary_location omega). All three describe the same drill/inspection apparatus but diverge on the empirical claim of whether exercised knowledge actually transfers, and so are linked here rather than merged into one measurement-parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
