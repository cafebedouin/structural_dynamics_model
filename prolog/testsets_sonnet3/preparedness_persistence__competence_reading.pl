% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Disaster Preparedness Drills and Inspections as Live Exercised Competence
 *   domain: institutional/safety/coordination
 *
 * SUMMARY:
 *   This constraint instantiates the competence reading of the
 *   preparedness_persistence kernel: drills and inspections as genuinely
 *   live-exercised knowledge whose repetition maintains real operational
 *   readiness. Under this reading, evacuation drills convert written
 *   procedure into rehearsed motor memory that functions under crisis stress,
 *   and inspections re-test physical mechanisms rather than reviewing
 *   paperwork. The structure is Mountain-adjacent (the physical readiness gap
 *   between drilled and undrilled response is a fact about human cognition
 *   and equipment degradation under crisis conditions, not a constructed
 *   rule) blended with Rope (the scheduling, funding, and coordination of who
 *   drills when is a genuine multi-party coordination problem, cleanly solved
 *   with minimal coercive overhead). Extraction is authored low and flat
 *   across the interval because this reading asserts the coordination
 *   function has not decayed into extraction or performance — the low, stable
 *   theater_ratio and base_extractiveness values are the reading's central
 *   empirical claim, not an artifact of insufficient data. The sibling
 *   readings (husk_reading: form persists while competence atrophies;
 *   hybrid_reading: stratified competence across components) are NOT
 *   represented here — they are separate constraint files with their own ε
 *   values, per the ε-invariance principle. This reading's ε is stable and
 *   low because its claim is that the constraint IS what it says it is.
 *
 * KEY AGENTS:
 *   - safety_inspectors: agenda_setter (institutional/constrained) — administers and personally re-verifies physical function
 *   - drill_participants: beneficiary/payer (moderate/constrained) — bear small time cost, gain rehearsed readiness
 *   - emergency_responders: beneficiary (organized/constrained) — rely on drilled familiarity for effective rescue
 *   - facility_operators: agenda_setter/payer (powerful/mobile) — fund compliance, liable for genuine failure
 *   - surrounding_communities: beneficiary (organized/constrained) — bear tail risk if readiness is false
 *   - regulatory_agencies: observer (institutional/analytical) — audits outcomes against drilled claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.08).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.1).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Disaster Preparedness Drills and Inspections as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "institutional/safety/coordination").

domain_priors:requires_active_enforcement(preparedness_persistence__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '5482f5ea-e742-4d0c-a8c1-974c4590f593').
narrative_ontology:cs_kernel_codification('5482f5ea-e742-4d0c-a8c1-974c4590f593', formalized).
narrative_ontology:cs_authority_grounding('5482f5ea-e742-4d0c-a8c1-974c4590f593', expertise).
narrative_ontology:cs_interpretation_layer_present('5482f5ea-e742-4d0c-a8c1-974c4590f593').
narrative_ontology:cs_reading_relation('5482f5ea-e742-4d0c-a8c1-974c4590f593', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('5482f5ea-e742-4d0c-a8c1-974c4590f593', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('5482f5ea-e742-4d0c-a8c1-974c4590f593', foundational, rehearsed_practice_transfers_to_crisis_performance).
narrative_ontology:cs_axiom_status(rehearsed_practice_transfers_to_crisis_performance, holdable).
narrative_ontology:cs_axiom_grounding('5482f5ea-e742-4d0c-a8c1-974c4590f593', rehearsed_practice_transfers_to_crisis_performance, empirically_contingent).
narrative_ontology:cs_axiom('5482f5ea-e742-4d0c-a8c1-974c4590f593', secondary, inspection_re_verifies_physical_function_rather_than_documents).
narrative_ontology:cs_axiom_status(inspection_re_verifies_physical_function_rather_than_documents, holdable).
narrative_ontology:cs_axiom_grounding('5482f5ea-e742-4d0c-a8c1-974c4590f593', inspection_re_verifies_physical_function_rather_than_documents, empirically_contingent).
narrative_ontology:cs_reference_frame('5482f5ea-e742-4d0c-a8c1-974c4590f593', post_incident_reform_baseline).
narrative_ontology:cs_drift_state('5482f5ea-e742-4d0c-a8c1-974c4590f593', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5482f5ea-e742-4d0c-a8c1-974c4590f593', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, building_occupants).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, facility_operators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, surrounding_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, drill_participants).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, drill_participants).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, facility_operators).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, practice_maintains_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct scheduled and surprise inspections of fire suppression systems, structural load points, evacuation routes, and equipment function. Their findings gate occupancy permits and insurance eligibility. They personally re-test mechanisms rather than reviewing paperwork, and their professional standing depends on inspections catching real deficiencies rather than rubber-stamping.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, safety_inspectors, agenda_setter,
    institutional, generational, constrained, regional).

% Building occupants, plant workers, and school staff who lose working time to periodic evacuation drills and equipment familiarization. In exchange they gain rehearsed motor memory of routes and procedures that functions under genuine crisis stress, when deliberative planning is unavailable. Their cost is real but small relative to the readiness gained.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, drill_participants, beneficiary,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, drill_participants, payer).

% Fire departments, paramedics, and rescue teams rely on the drilled familiarity of building occupants and the verified function of on-site systems (alarms, sprinklers, marked egress) to execute rescues efficiently. A building whose drills are genuinely exercised is measurably faster and safer to respond to than one whose procedures exist only on paper.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_responders, beneficiary,
    organized, biographical, constrained, regional).

% Own or manage the buildings and industrial sites subject to inspection. They schedule and fund the drills, bear the direct cost of inspection compliance and any required remediation, and can in principle under-invest in genuine exercises — but a genuine failure exposes them to liability, insurance loss, and reputational and criminal consequence, which keeps their incentive aligned with real readiness rather than performance.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, facility_operators, agenda_setter,
    powerful, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, facility_operators, payer).

% Neighbors, downstream residents, and dependent infrastructure that would be directly exposed to industrial or structural failure. They rarely observe drills directly but bear the tail-risk consequence of them failing, and benefit from a facility whose readiness is real rather than nominal.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, surrounding_communities, beneficiary,
    organized, generational, constrained, regional).

% Set the inspection standards, audit inspector findings, and investigate post-incident whether the drilled procedures actually functioned. They compile after-action data comparing drilled performance to real-incident outcomes, which is the primary corroborating evidence for whether this reading of the constraint holds.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of maintaining rehearsed, verified operational readiness for low-frequency, high-consequence events (fires, structural failure, industrial accidents) where deliberative planning under crisis conditions is unreliable. Repetition converts declarative knowledge (a written evacuation plan) into procedural knowledge (bodies that move correctly under stress) and surfaces latent equipment failures before they matter.
% TRANSFER_FUNCTION: Moves scheduled time and inspection cost from occupants and operators into verified readiness capital held jointly by occupants, responders, and the surrounding community; there is no asymmetric rent extraction identified in this reading — costs are borne by the same parties who receive the safety benefit.
% ABSENT_VOICES: No systematically excluded party is identified under this reading; workers with disabilities or non-native language speakers may experience drills as less effective for them specifically, but this is a design-quality gap within the coordination function, not an extraction structure directed at them.
% DISAPPEARANCE_RATIONALE: If drills and inspections vanished overnight, equipment failures would go undetected until a real event, evacuation behavior would revert to unrehearsed panic response, and response times and casualty rates in a genuine incident would measurably worsen — insurance underwriting, occupancy permitting, and emergency response protocols are all structured around the assumption that this readiness is real and current.
% FOUNDING_PROBLEM: Early industrial and structural fires killed occupants who did not know evacuation routes and were rescued by responders who did not know a building's layout or hazard profile, because neither had ever rehearsed against the specific site.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and independent after-action investigations (post-incident reviews conducted by bodies with no financial stake in the facility's continued operation) corroborate that facilities with genuinely exercised drills and passed inspections show measurably better real-incident outcomes than facilities with lapsed or performative programs — this is the evidentiary basis distinguishing this reading from the husk reading, not self-report from facility operators.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).
:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) and essentially flat because this reading's structural claim is that costs (inspection time, drill disruption) are borne by the same parties who receive the safety benefit — there is no identified party who collects rent from others' compliance. Theater ratio is authored low (0.12) because the reading asserts inspectors physically re-test mechanisms and drills produce genuine rehearsed behavior, not paperwork exercises. Suppression is low (0.1): compliance is enforced (permits, insurance, liability) but the enforcement backs a function participants would rationally want even absent enforcement, distinguishing this from coercive extraction. Accessibility_collapse is moderate (0.35), reflecting that alternatives to drilling (e.g., relying on written plans alone) technically exist but are known to fail catastrophically under real crisis conditions — this is closer to the mountain end than a pure convention would be, without claiming full physical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder is authored with high directionality toward extraction under this reading, because no victim group exists. Facility operators sit closest to a cost-bearing position (fund compliance, absorb remediation cost) but their exit is mobile and their liability exposure aligns their interest with genuine readiness rather than performance, so they are not treated as targets. Drill participants and communities are net beneficiaries whose small costs are the price of a real risk reduction, consistent with Rope's requirement that participants are net beneficiaries and alternatives are not suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (occupants and responders dying because neither rehearsed the specific site) is authored as live, not dead — fires, structural failures, and industrial accidents remain live risks, and this reading's corroboration comes from independent post-incident review comparing drilled versus undrilled outcomes, not from facility self-report. Because founding_problem_status is 'live' and disappearance_verdict is 'world_rearranges', the mismatch-detection consumer finds no capture/zombie signal here — this is exactly the profile a genuinely functioning coordination mechanism should show, contrasted against the husk_reading sibling where the same mismatch check would be expected to fire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_performance_drift,
    'Does repeated drilling and inspection at this facility genuinely maintain operational competence over time, or has it drifted toward memorial performance (the husk_reading''s claim) without this reading''s stakeholders noticing the drift?',
    'Longitudinal after-action comparison: track real-incident outcomes (evacuation time, casualty rate, equipment function under actual crisis) against drilled/inspected status over a multi-decade window, using regulatory agency data independent of facility self-report.',
    'If outcomes diverge from drilled expectations over time, this reading''s low, flat extractiveness and theater_ratio series would be shown to be a snapshot mistaken for the steady state, and the constraint would need reclassification toward the husk_reading or hybrid_reading profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_performance_drift, empirical, 'Whether the competence reading''s claim of stable, non-decaying readiness holds under longitudinal outcome data.').

omega_variable(
    component_stratification_ambiguity,
    'Is the readiness-maintenance function uniform across all inspected/drilled components, or does it stratify — with engineering inspection (physical, verifiable) remaining competent while evacuation drills (behavioral, harder to verify) ritualize, as the hybrid_reading claims?',
    'Disaggregate outcome and process data by component type (mechanical system inspection vs. human evacuation drill) rather than treating ''preparedness'' as a single aggregate measure.',
    'If stratification is real and substantial, this single-reading story oversimplifies by treating the whole facility''s preparedness apparatus as uniformly competent; the hybrid_reading would be the more structurally accurate decomposition for at least some facilities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_stratification_ambiguity, conceptual, 'Whether the kernel''s competence claim should be evaluated at the level of the whole facility or decomposed by component.').

omega_variable(
    enforcement_dependence_on_readiness_value,
    'Does the active enforcement (permits, insurance, liability) that backs this constraint exist because participants independently value real readiness, or does the enforcement itself manufacture apparent compliance that could mask decay?',
    'Examine facilities in low-enforcement jurisdictions or exempted categories to see whether drill/inspection quality holds without the liability backstop.',
    'If quality collapses without enforcement, the Rope characterization (participants are net beneficiaries who would coordinate voluntarily) weakens, and the constraint moves closer to a Tangled Rope where enforcement is load-bearing rather than incidental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_dependence_on_readiness_value, conceptual, 'Whether enforcement is incidental scaffolding around voluntary coordination or a necessary prop holding up an otherwise-decaying function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__competence_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__competence_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__competence_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__competence_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__competence_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__competence_reading, base_extractiveness, 8, 0.06).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__competence_reading, base_extractiveness, 16, 0.07).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__competence_reading, base_extractiveness, 24, 0.07).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__competence_reading, base_extractiveness, 32, 0.08).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__competence_reading, base_extractiveness, 40, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__competence_reading, suppression_requirement, 0, 0.09).
narrative_ontology:measurement(prep_su_t8, preparedness_persistence__competence_reading, suppression_requirement, 8, 0.09).
narrative_ontology:measurement(prep_su_t16, preparedness_persistence__competence_reading, suppression_requirement, 16, 0.09).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__competence_reading, suppression_requirement, 24, 0.1).
narrative_ontology:measurement(prep_su_t32, preparedness_persistence__competence_reading, suppression_requirement, 32, 0.1).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__competence_reading, suppression_requirement, 40, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the preparedness_persistence kernel. competence_reading (this file) authors low, stable ε reflecting genuine maintained readiness. husk_reading authors high theater_ratio and accessibility_collapse reflecting memorial performance with atrophied underlying competence. hybrid_reading authors component-stratified metrics, treating engineering inspection and evacuation drills as structurally distinct sub-claims with different ε profiles. All three share the same underlying institutional apparatus (drills, inspections, permits) but diverge in what they claim is actually happening inside it — per the ε-invariance principle, this divergence is why they are three files, not one file with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
