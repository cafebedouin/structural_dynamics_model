% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: institutional/organizational/disaster_management
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness_commitment kernel: an institution's drill cycle — the
 *   repeated, evaluated rehearsal of emergency procedures across staff
 *   generations — is here read as genuinely functional. In this reading,
 *   drills test real decision-making under simulated stakes, performance data
 *   feeds back into procedure revision, and newly onboarded responders reach
 *   a demonstrable competence bar before veterans rotate out. The
 *   distinguishing empirical claim of this reading is that drill performance
 *   predicts real-incident performance, verified by independent auditors
 *   rather than self-report. This is not the same constraint as the
 *   husk_reading (where the same-looking drill calendar produces only the
 *   feeling of retention without operational transfer) or the hybrid_reading
 *   (where memorial and competence elements are structurally layered) — those
 *   are separate constraints with their own ε, authored as separate stories
 *   and linked here via network.affects_constraints, per the ε-invariance
 *   principle. Low, stable extraction and low suppression reflect a genuinely
 *   coordinative, voluntarily-reinforced arrangement rather than an
 *   extractive one.
 *
 * KEY AGENTS:
 *   - training_cadre: designs and iterates the drill cycle based on live performance data — organized/constrained
 *   - frontline_responders: pay in effort, receive real transferable skill — moderate/constrained
 *   - affected_populations: ultimate beneficiaries of the competence the drills actually produce — powerless/trapped
 *   - incoming_cohort_members: successfully absorb institutional knowledge through tested transmission — moderate/constrained
 *   - independent_auditors: external verification that drill outcomes track real-incident outcomes — institutional/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.22).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "institutional/organizational/disaster_management").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '3959b8ef-72ed-47c0-a901-65da826c4258').
narrative_ontology:cs_kernel_codification('3959b8ef-72ed-47c0-a901-65da826c4258', implicit).
narrative_ontology:cs_authority_grounding('3959b8ef-72ed-47c0-a901-65da826c4258', practice).
narrative_ontology:cs_interpretation_layer_present('3959b8ef-72ed-47c0-a901-65da826c4258').
narrative_ontology:cs_reading_relation('3959b8ef-72ed-47c0-a901-65da826c4258', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('3959b8ef-72ed-47c0-a901-65da826c4258', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('3959b8ef-72ed-47c0-a901-65da826c4258', foundational, drill_performance_causally_predicts_incident_performance).
narrative_ontology:cs_axiom_status(drill_performance_causally_predicts_incident_performance, holdable).
narrative_ontology:cs_axiom_grounding('3959b8ef-72ed-47c0-a901-65da826c4258', drill_performance_causally_predicts_incident_performance, empirically_contingent).
narrative_ontology:cs_axiom('3959b8ef-72ed-47c0-a901-65da826c4258', secondary, generational_transmission_requires_tested_not_merely_attended_practice).
narrative_ontology:cs_axiom_status(generational_transmission_requires_tested_not_merely_attended_practice, holdable).
narrative_ontology:cs_axiom_grounding('3959b8ef-72ed-47c0-a901-65da826c4258', generational_transmission_requires_tested_not_merely_attended_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('3959b8ef-72ed-47c0-a901-65da826c4258', founding_era_operational_drilling).
narrative_ontology:cs_drift_state('3959b8ef-72ed-47c0-a901-65da826c4258', contemporary_accredited_program, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3959b8ef-72ed-47c0-a901-65da826c4258', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, affected_populations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, training_cadre).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, incoming_cohort_members).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, incoming_cohort_members).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, operational_competence_transmits_across_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and runs the drill cycle, writes after-action reports, and updates procedures based on what actually failed in live exercises. Their authority rests on demonstrated capacity to produce responders who perform under real conditions, not on tenure or ceremony. They can be replaced if drills stop producing competent responders.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, training_cadre, agenda_setter,
    organized, generational, constrained, regional).

% Undergo repeated, effortful, sometimes physically demanding drills that simulate genuine decision pressure. They pay in time and discomfort but receive real transferable skill — the drills change what they would actually do in a crisis, verified by scenario performance, not attendance.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, frontline_responders, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, frontline_responders, payer).

% Are the people whose survival depends on responders' actual competence during an emergency. They have no direct role in the drill cycle but are the ultimate recipients of whatever operational capacity the routines actually produce.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, affected_populations, beneficiary,
    powerless, immediate, trapped, regional).

% New personnel who must absorb the institution's accumulated operational knowledge as veterans retire or rotate out. Under this reading, the transmission actually works: successor cohorts are tested against live scenarios and demonstrably reach the competence bar, not merely inherit the paperwork.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, incoming_cohort_members, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, incoming_cohort_members, payer).

% External bodies (inspectors general, accreditation reviewers, post-incident investigators) who evaluate whether drill outcomes correlate with real emergency performance. Their assessments are the primary evidence for whether this reading — as opposed to the husk reading — is the accurate account of a given program.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, independent_auditors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a population of responders capable of executing complex, time-critical, low-frequency emergency procedures correctly, by repeatedly rehearsing those procedures under conditions that approximate real stakes and testing whether performance actually improves.
% TRANSFER_FUNCTION: Moves effort and discomfort (time spent drilling, cognitive load of simulated crisis) from responders to themselves in exchange for skill; moves risk reduction from the training cadre's design work to the affected population who benefit from competent response when an actual event occurs.
% ABSENT_VOICES: Future victims of the next disaster are not present to demand that drills test real decisions rather than checklist completion; their interests are represented only proxy-wise through auditors and after-action review, which may itself be captured or under-resourced.
% DISAPPEARANCE_RATIONALE: If the drill cycle vanished, operational competence would decay within one or two personnel turnover cycles — institutional knowledge that exists only in trained reflexes and tested judgment does not persist in manuals alone. Emergency response quality would degrade measurably, and affected populations would face materially worse outcomes in the next crisis.
% FOUNDING_PROBLEM: Low-frequency, high-consequence emergencies (fires, floods, mass-casualty events, infrastructure failures) cannot be learned from direct experience alone, because by the time an individual responder has lived through enough real events to be reliably competent, the personal cost of the learning curve has already been paid in failures. Drilling substitutes rehearsed, evaluated practice for that costly experience curve.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-incident review boards and accreditation auditors — bodies with no stake in the training cadre's budget or reputation — attest that, in programs matching this reading, drill performance metrics predict real-incident outcomes; this is the outside-the-benefiting-parties corroboration that distinguishes the competence reading from the husk reading for any specific program.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.18) and essentially flat across the interval because, under this reading, the coordination function is real and the costs borne by responders (time, discomfort) are proportionate to the skill they receive — there is no rent extracted from a captive population. Suppression is low-moderate (0.22): some mandatory participation exists (drills are compulsory, not optional), but this is coordination overhead rather than coercive extraction, since the beneficiary class (affected populations) and the paying class (responders) substantially overlap or are aligned rather than opposed. Theater ratio is deliberately kept low (0.12) and nearly flat, which is the defining metric signature that distinguishes this reading from the husk_reading — in this reading, the performative-to-functional ratio stays low because drills are actually testing decisions, not merely being staged for compliance optics.
 *
 * DIRECTIONALITY LOGIC:
 *   The training cadre sits near symmetric-to-beneficiary: they invest effort in design but their institutional standing depends on producing real competence, which is itself validated externally. Frontline responders and incoming cohort members carry moderate cost (time, cognitive load) for real returns (transferable skill, career competence), placing them close to symmetric rather than as targets. Affected populations are pure beneficiaries with no direct cost — they receive the downstream benefit of competent response without paying into the system. No agent is authored as a victim in this reading, consistent with the expected structural delta: the D5 generational-transfer break is avoided or contained, so there is no extraction to assign.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is precisely the case the classification system must NOT collapse into 'all institutional ritual is theater.' Because drill performance is independently verified against real-incident outcomes (by auditors outside the training cadre's own reporting chain), this reading has a genealogy that is falsifiable — if performance data stopped predicting real outcomes, the program would drift toward the husk_reading, not stay classified as competence. The mandatrophy risk this reading resists is treating a functioning coordination mechanism as automatically suspect merely because it resembles ceremonial repetition from the outside; the corroboration requirement is what prevents that misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_infrastructure_dependency,
    'Does the independent auditor function (post-incident review boards, accreditation bodies) that corroborates this reading actually exist with sufficient independence and resourcing in the specific program this story describes, or is ''independent verification'' itself partly captured by the training cadre?',
    'Trace the funding source, appointment process, and publication independence of the auditing body; compare auditor findings against raw incident-response time-series data collected by a third party with no institutional relationship to either the cadre or the auditors.',
    'If the verification infrastructure is weak or captured, this story''s central empirical claim (drill performance predicts real-incident performance) becomes unfalsifiable in practice, and the program should be re-evaluated against the husk_reading or hybrid_reading rather than assumed to be this reading by default.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_infrastructure_dependency, empirical, 'Whether the auditor corroboration this reading depends on is itself structurally independent.').

omega_variable(
    drift_from_competence_to_husk,
    'Given generational turnover in the training cadre itself, how stable is the competence_reading over multi-decade horizons — does the cadre''s own tacit knowledge of what makes a drill ''real'' erode even as the drill calendar''s formal structure persists unchanged?',
    'Longitudinal tracking of drill design revision frequency and substance (not just occurrence) against personnel turnover in the training cadre; a cadre that stops revising drills based on after-action findings is exhibiting early husk drift regardless of current classification.',
    'If competence transmission depends on tacit knowledge held by specific senior trainers who are themselves subject to turnover, this reading may be a snapshot of a program mid-transition toward the husk_reading, not a stable steady state — this bears directly on how much weight to give a single point-in-time classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_from_competence_to_husk, conceptual, 'Whether the competence reading is a stable attractor or a transitional state that decays toward the husk reading absent active maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__competence_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__competence_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__competence_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__competence_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__competence_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__competence_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__competence_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__competence_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(prep_su_t8, preparedness_commitment__competence_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(prep_su_t16, preparedness_commitment__competence_reading, suppression_requirement, 16, 0.21).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__competence_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement(prep_su_t32, preparedness_commitment__competence_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__competence_reading, 0.08).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'preparedness commitment' per the ε-invariance principle: measuring the same institutional drill calendar through the lens of 'does it actually transfer competence' (this story, low ε) versus 'does it merely feel like retention' (husk_reading, high ε — the drills become extraction of compliance-time from responders and false assurance sold to affected populations) versus 'are memorial and competence functions layered' (hybrid_reading, intermediate ε) yields three structurally distinct constraints, not one constraint viewed three ways. Each carries its own beneficiary/victim structure and its own claimed_type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
