% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Preparedness Ritual Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint is one reading of a contested kernel about disaster
 *   preparedness: whether drills and inspections constitute live, exercised
 *   knowledge (competence_reading) or memorial performance where form
 *   persists while operational capacity atrophies (husk_reading). The husk
 *   reading asserts that the apparatus persists not because it maintains
 *   readiness, but because the institutional beneficiaries — bureaucratic
 *   agencies, political leaders, inspection auditors — profit from the
 *   appearance of preparedness without bearing the cost of failures. The
 *   constraint operates through suppression: operational staff who see the
 *   gap are muted by the institutional pressure to validate compliance;
 *   flood-exposed populations lack information about actual readiness; senior
 *   commanders' frank operational assessments are subordinated to compliance
 *   metrics. The theater_ratio rises over the interval as the gap between
 *   formal procedures and actual capacity grows and becomes normalized — the
 *   performance becomes ever more elaborate (documentation, certification,
 *   inter-agency coordination theater) while functional capacity degrades.
 *   The measuring interval captures the post-founding-problem era when the
 *   ritual became decoupled from live practice.
 *
 * KEY AGENTS:
 *   - emergency_management_bureaucracy: institutional agenda-setter; designs drills, receives compliance attestations, derives legitimacy from preparedness appearance.
 *   - flood_exposed_populations: powerless victims; live in flood zones under assumption of readiness; trapped by economic/social ties; bear consequence of system failure.
 *   - junior_operational_staff: moderate-power payers; see the gap between procedure and capacity; constrained exit; voice concerns but institutional structure incentivizes silence.
 *   - senior_operation_commanders: institutionally placed but excluded from planning; know the actual state of readiness; constrained by pressure to validate official narrative.
 *   - political_leadership: beneficiaries; gain plausible deniability through compliance documentation; no operational responsibility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.72).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.79).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.79).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Preparedness Ritual Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_persistence__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, 'aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2').
narrative_ontology:cs_kernel_codification('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', formalized).
narrative_ontology:cs_authority_grounding('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', extraction).
narrative_ontology:cs_interpretation_layer_present('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2').
narrative_ontology:cs_reading_relation('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', foundational, drills_inspections_form_divorced_from_function).
narrative_ontology:cs_axiom_status(drills_inspections_form_divorced_from_function, holdable).
narrative_ontology:cs_axiom_grounding('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', drills_inspections_form_divorced_from_function, empirically_contingent).
narrative_ontology:cs_axiom('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', foundational, institutional_beneficiary_perpetuates_theater).
narrative_ontology:cs_axiom_status(institutional_beneficiary_perpetuates_theater, holdable).
narrative_ontology:cs_axiom_grounding('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', institutional_beneficiary_perpetuates_theater, empirically_contingent).
narrative_ontology:cs_reference_frame('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', post_founding_coordination_establishment).
narrative_ontology:cs_drift_state('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', contemporary_25year_mark, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aea9c7fa-8884-4fbe-b36e-7af6f1b6fff2', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_bureaucracy).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, flood_exposed_populations).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, junior_operational_staff).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, drill_inspection_auditors).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, political_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates drills and inspections; compiles compliance reports; maintains the institutional apparatus that attests readiness. Does not itself deploy into flood zones or coordinate first-response operations. Derives institutional legitimacy and budgetary justification from the appearance of preparedness. Has substantial discretion over what counts as 'successful' drill or 'passing' inspection — the metrics are internal to the apparatus.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Live in flood-prone areas under the assumption that emergency management systems are operationally ready. Have no ability to test the actual competence of response systems before a flood occurs. Bear the consequence if the system fails — displacement, loss of property, loss of life. Cannot leave the region due to economic and social ties.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, flood_exposed_populations, payer,
    powerless, biographical, trapped, regional).

% Work as first responders, shelter operators, or evacuation coordinators. Participate in drills and inspections knowing the procedures have atrophied from practice. See the gap between the official training protocols and actual equipment, communication systems, and coordination capacity. Cannot exit without career cost; voice concerns but institutional structure incentivizes reporting compliance rather than operational gaps.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, junior_operational_staff, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, junior_operational_staff, observer).

% Conduct inspections and certify drills as 'passing.' Benefit from continued demand for their services by the institutional framework. Operate under directives that define success as procedural adherence rather than operational competence testing. Have professional incentive to validate the system rather than report critical gaps.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, drill_inspection_auditors, agenda_setter,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, drill_inspection_auditors, beneficiary).

% Can point to compliance with national preparedness standards and cite drill/inspection reports as evidence of readiness. If a flood occurs and response fails, the documentation provides plausible deniability: procedures were followed, drills were conducted, inspections were passed. Benefits from the appearance of preparedness without bearing operational responsibility.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, political_leadership, beneficiary,
    institutional, biographical, mobile, regional).

% Manage actual deployment in crisis. Would attest that equipment is degraded, communication networks are fragmented, coordination protocols are theoretical, and the organizational muscle memory for large-scale response has eroded. Their frank assessment is muted by institutional pressure to validate the official readiness narrative. They are partially excluded from the planning process; their operational insights compete with compliance metrics.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, senior_operation_commanders, excluded,
    institutional, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, emergency_management_bureaucracy).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common framework for what 'preparedness' means: standardized procedures, regular testing, documented capability baselines. Solves the coordination problem of aligning disparate regional emergency systems under national protocols.
% TRANSFER_FUNCTION: Transfers institutional legitimacy from the bureaucratic apparatus to the state; transfers false confidence in operational readiness to the population at risk. Extracts deference and resources (funding, time, compliance labor) from operational staff and flood-exposed communities in exchange for the symbolic appearance of readiness, not demonstrable capacity.
% ABSENT_VOICES: Senior operational commanders who understand the gap between official procedures and actual capability; flood-affected residents from the last major disaster who experienced the system's actual performance; independent emergency-management experts from outside the benefiting bureaucracy who could audit real readiness rather than procedural compliance.
% DISAPPEARANCE_RATIONALE: If the drill-and-inspection apparatus vanished, the institutional basis for claiming preparedness would collapse. Regional governments would face pressure to conduct actual competence-based testing or genuine training refresh. Resources would shift from compliance theater to operational improvement. The population's implicit assumption of readiness would be shattered, likely triggering either real preparedness investment or public acknowledgment of the risk. The system would reorganize around demonstrable capacity rather than procedural attestation.
% FOUNDING_PROBLEM: After major flood disasters revealed coordination gaps and communication failures, national governments established standardized emergency-management protocols and regular testing to ensure consistent readiness and inter-regional coordination.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — lack of coordination standards and testing infrastructure — is attested as solved by bureaucratic institutional actors. Senior operational commanders and emergency-management scholars outside the benefiting apparatus attest that the founding problem has been superseded: the real problem now is the gap between mandated procedures and actual operational capacity, which the ritual inspection apparatus actively obscures rather than solves. Flood-response after-action reports from recent events document the disparity.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The husk reading declares this constraint a PITON: a former coordination function (establishing national preparedness standards) has atrophied into performative compliance. The theater_ratio trajectory (0.62 → 0.79) traces the growing disparity between formal drills/inspections (increasingly elaborate, well-documented, certified) and actual operational capacity (equipment degraded, communication fragmented, institutional memory eroded). The measured theater_ratio floor at 0.79 by interval end reflects a constraint operating almost entirely through procedural attestation rather than demonstrated competence. Suppression (0.72) is substantial because the apparatus actively mutes operational staff who report gaps — institutions redirect concerns into 'improvement plans' that generate more theater. Extractiveness (0.68) is high because the system extracts deference, resources, and false confidence from the population at risk in exchange for ceremonial readiness, not real preparedness. The accessibility_collapse (0.42) is moderate-low because alternatives EXIST and are visible to those inside the system — the junior staff and operational commanders know the procedures are theoretical; they could refuse to participate or blow the whistle, but the institutional cost is high and the beneficiary's suppression machinery makes exit costly. The resistance (0.51) is moderate: junior staff push back (generating 'improvement initiatives' that stay cosmetic), public concern after near-miss floods rises temporarily, but no sustained challenge breaks through because the bureaucratic apparatus absorbs criticism and reconstitutes the theater. The claimed type (piton) reflects that this is no longer a mountain (natural, irreplaceable) nor a functional rope (genuine coordination) — it is an institutional practice maintained by inertia and the concentrated benefit to the agenda-setter despite diffuse cost to the payer.
 *
 * PERSPECTIVAL GAP:
 *   From the bureaucratic agenda-setter's seat, the apparatus IS preparedness: procedures are in place, drills happen on schedule, inspections certify compliance. The theater appears as functional form. From the junior operational staff and flood-exposed population's seat, the apparatus IS the problem: form has decoupled from capability, resources go to certification rather than real training, the population's vulnerability is masked by false confidence. The engine will compute this divergence from the structural data: the beneficiary (bureaucracy) sits near d=0 (collects benefit, no exit cost); the payer (population, junior staff) sits near d=1.0 (bears cost, trapped or heavily constrained exit). The gap between seats' computed types is the measurement the framework exists to capture — a constraint CLAIMED as piton that computes differently across seats reveals the asymmetric extraction the husk reading exposes.
 *
 * DIRECTIONALITY LOGIC:
 *   The agenda-setter (emergency_management_bureaucracy) benefits from the continuation of the apparatus without needing to improve actual capacity: d ≈ 0.1 (pure beneficiary, high institutional power, arbitrage exit if need be, can simply recalibrate what counts as readiness). The payer seats diverge: flood_exposed_populations are d ≈ 1.0 (trapped, powerless, receive no direct benefit, bear existential risk); junior_operational_staff are d ≈ 0.85 (constrained exit, moderate power, see the gap and bear responsibility for the gap's consequences without authority to fix it); political_leadership are d ≈ 0.25 (beneficiary from plausible deniability, but institutionally positioned so bear some reputational cost if catastrophic failure occurs — not a full target, but held by the apparatus). Senior_operation_commanders are d ≈ 0.65 (institutionally excluded, would be hit by failure consequences, constrained exit, but retain some professional status and information asymmetry advantage). The directionality derivation from victims (flood_exposed, junior_staff) + beneficiaries (bureaucracy, political_leadership) + power + exit_options produces this spectrum. Override: senior_operation_commanders nominally 'organized' institutional power, but their role is 'excluded' and their actual structural relationship is as a constrained target (they will bear operational responsibility for the system's failure in ways the agenda-setter will not) — no override needed because the structural data already specifies 'excluded,' but note the specification captures the constraint's asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (mandate outliving function) is explicit in this reading. The founding mandate — establish national emergency-management standards to ensure coordination and readiness — is accomplished: standards exist, procedures are uniform, inter-agency coordination is formal. But the apparatus continues and THICKENS because the institutional beneficiaries profit from it without running the risk it was meant to manage. The constraint persists through theater because: (1) dismantling the apparatus would admit publicly that preparedness was theater, creating reputational cost for political leadership and budgetary cost for the bureaucratic agenda-setter; (2) the payers (populations, junior staff) lack the power to demand actual competence instead of ritual; (3) the suppression machinery (institutional pressure, career consequences, information asymmetry) mutes the operational actors who know the gap. The piton classification captures this precisely: the constraint is maintained not by its function (that is dead or atrophied) but by institutional inertia and the concentrated benefit to those who administer it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_operational_capacity_unknown,
    'What is the actual, tested operational capacity of this emergency management system under large-scale real-world flood conditions? How divergent is it from the capacity implied by passed inspections and successful drill performances?',
    'Post-event analysis after the next major flood: compare actual system performance to pre-event certification records and drill results. Independent external audit of equipment state, communication network integrity, and coordination protocols using conditions that match real-world constraints (equipment failures, communication degradation, resource scarcity). Competence-based testing rather than procedural compliance testing.',
    'If the gap is large (actual capacity substantially below certified capacity), the constraint shifts structurally from piton toward snare — the apparatus is actively deceiving the population rather than passively performing ritual. If the gap is small, the constraint may shift toward the hybrid_reading (some components competent, others ritualized). The size of the gap determines whether this reading or the competence_reading dominates empirically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(actual_operational_capacity_unknown, empirical, 'The unknown actual operational capacity of the preparedness apparatus, concealed by the theater of inspection.').

omega_variable(
    institutional_incentive_structure_persistence,
    'Are the beneficiary institutional actors (bureaucracy, political leadership, audit agencies) structurally locked into defending the theater, or could they transition to genuine competence-based assessment without career/budgetary cost?',
    'Policy experiment: establish independent, competence-based assessment alongside or replacing compliance-based certification. Track whether institutional actors resist, redefine success, or reorganize. Analyze career consequences for those who advocate genuine competence testing. Historical analysis of past attempts at competence-based reform and what happened.',
    'If actors are locked in (career cost for admission of gaps is high), the theater persists indefinitely — suppression remains high, extraction continues. If transition is possible but costly (requires reframing, admits past failure), the apparatus might shift toward hybrid_reading (some honest assessment mixed with retained theater). If transition is low-cost, the reading might shift toward competence_reading or disintegrate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_structure_persistence, preference, 'Whether institutional beneficiaries can exit the theater without career/political cost, or are locked into defending it.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression that keeps junior operational staff from reporting the gap between procedure and capacity STRUCTURAL (external institutional pressure, career consequences, authority hierarchies) or INTERNALIZED (staff have adopted the belief that procedures are adequate, or have identity-fused with the apparatus)?',
    'Post-exit analysis: track junior staff who leave the system — do they continue believing the procedures are adequate, or do they revise? Anonymous reporting mechanisms that remove institutional pressure — what do staff report about actual capacity when not under observation? Interviews with staff who have experienced both inside and outside the system. Comparison with staff in hybrid systems that are more transparent about gaps.',
    'If suppression is structural, removing institutional pressure (independent agency, whistleblower protection, transparent reporting) would shift the narrative quickly. If suppression is internalized, the constraint persists even after people leave the apparatus — they carry the belief with them. Internalized suppression suggests the apparatus has actively trained people to conflate form with function, requiring re-education alongside institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural institutional pressure or internalized belief, with different remediation paths.').

omega_variable(
    kernel_reading_empirical_adjudication,
    'Is the husk_reading or the competence_reading more empirically accurate about the actual state of this specific preparedness apparatus?',
    'Post-event analysis from recent major disasters where this apparatus was deployed. Comparison of predicted vs. actual system performance. External audit of equipment, communication, coordination readiness by researchers outside the benefiting institution. Analysis of lesson-learned reports and how many lessons are actually implemented. Frequency of discovered failures in drills vs. operational deployments.',
    'If competence_reading is accurate, the husk reading is a reading imposed by cynics/researchers and the constraint should compute toward competence-verified rope or mountain (genuine coordination). If husk_reading is accurate, the constraint is correctly classified as piton with high theater and should compute toward snare from the payer seats. The kernel contest is partly empirical — the readings make factual claims about the system''s actual operational state, not just normative claims about what matters.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_empirical_adjudication, empirical, 'Whether the apparatus is actually competent (competence_reading) or actually atrophied (husk_reading) — an empirical question about real-world operational performance that only real deployment reveals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(prep_tr_t3, preparedness_persistence__husk_reading, theater_ratio, 3, 0.67).
narrative_ontology:measurement(prep_tr_t6, preparedness_persistence__husk_reading, theater_ratio, 6, 0.71).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__husk_reading, theater_ratio, 10, 0.75).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__husk_reading, theater_ratio, 15, 0.78).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__husk_reading, theater_ratio, 20, 0.81).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__husk_reading, theater_ratio, 25, 0.79).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(prep_be_t3, preparedness_persistence__husk_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(prep_be_t6, preparedness_persistence__husk_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__husk_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__husk_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__husk_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__husk_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__husk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(prep_su_t3, preparedness_persistence__husk_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(prep_su_t6, preparedness_persistence__husk_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__husk_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(prep_su_t15, preparedness_persistence__husk_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__husk_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(prep_su_t25, preparedness_persistence__husk_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__husk_reading, 0.18).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three constraint stories, each instantiating a different reading of whether drills/inspections constitute live knowledge or memorial performance. Husk_reading (this story) asserts the apparatus is atrophied piton maintained by institutional beneficiaries; competence_reading asserts it is live rope; hybrid_reading asserts components diverge. The readings are not observables of one constraint — they are three different constraints authored by three different committer frames. Each has its own ε (husk: 0.68, high extraction), beneficiary structure (husk: bureaucracy + political leadership; competence: population + junior staff), and type (husk: piton; competence: rope). The network links them: husk_reading INFLUENCES both siblings by creating structural pressure that would dissolve them if widely believed. They COEXIST through different parties holding different readings. See preparedness_persistence__competence_reading.json and preparedness_persistence__hybrid_reading.json for the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__husk_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
