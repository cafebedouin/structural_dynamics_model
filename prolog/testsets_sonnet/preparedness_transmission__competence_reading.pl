% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Live-Exercised Disaster Preparedness Competence (Competence Reading)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness_transmission kernel: drills and inspections function as
 *   genuine live-exercised knowledge, where each generation of exercises
 *   actually re-validates operational capability through unscripted or
 *   scenario-varied practice. Under this reading, mandatory participation and
 *   inspection are low-suppression, low-theater coordination mechanisms with
 *   a real and continuously renewed function. This is deliberately the
 *   optimistic structural claim among three sibling readings of the same
 *   underlying institutional pattern — the husk_reading (ritualized
 *   hollowing) and hybrid_reading (stratified decay) are separate constraint
 *   stories with their own ε values, not alternative measurements of this
 *   one.
 *
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
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Live-Exercised Disaster Preparedness Competence (Competence Reading)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '793c18f8-b6b6-48e9-b72f-e81e1912dcb8').
narrative_ontology:cs_kernel_codification('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', distributed).
narrative_ontology:cs_authority_grounding('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', practice).
narrative_ontology:cs_interpretation_layer_present('793c18f8-b6b6-48e9-b72f-e81e1912dcb8').
narrative_ontology:cs_reading_relation('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', foundational, practiced_capability_thesis).
narrative_ontology:cs_axiom_status(practiced_capability_thesis, holdable).
narrative_ontology:cs_axiom_grounding('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', practiced_capability_thesis, empirically_contingent).
narrative_ontology:cs_axiom('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', secondary, scenario_variation_forces_genuine_adaptation).
narrative_ontology:cs_axiom_status(scenario_variation_forces_genuine_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', scenario_variation_forces_genuine_adaptation, empirically_contingent).
narrative_ontology:cs_reference_frame('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', post_incident_reform_baseline).
narrative_ontology:cs_drift_state('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', contemporary_multi_decade_operation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('793c18f8-b6b6-48e9-b72f-e81e1912dcb8', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, resident_population).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, drill_participants).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, infrastructure_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, infrastructure_operators).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, practiced_capability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates the drill and inspection calendar, sets scenario variation, and certifies capability based on live performance rather than paper compliance. Bears reputational and political cost if a real event exposes a gap the drills should have caught, which keeps the design honest rather than ceremonial.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_authority, agenda_setter,
    institutional, generational, constrained, national).

% Fire, medical, and rescue units participate in drills with varied scenarios and unannounced elements. They pay in time, disrupted schedules, and the discomfort of being evaluated, but gain measurable improvement in response coordination and personal confidence under pressure that shows up when real incidents occur.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_response_agencies, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, emergency_response_agencies, payer).

% Municipal staff, building wardens, and volunteer responders who physically run the exercises. Because scenarios are varied and not scripted to a fixed answer key, they must actually improvise, which is effortful but produces transferable skill rather than rote memorization of one drill sequence.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, drill_participants, beneficiary,
    moderate, biographical, constrained, local).

% Lives inside the jurisdiction the system protects. Cannot personally verify whether the drills are real or theatrical, but is the direct beneficiary if response capability holds during an actual disaster; bears the cost of any hollowing-out but has no independent means to audit it.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, resident_population, beneficiary,
    powerless, generational, trapped, regional).

% Utilities, hospitals, and transit operators subject to inspection regimes tied to the same live-validation logic. Absorb inspection costs and operational disruption but gain genuine assurance that failure modes are caught before they compound during a crisis.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, infrastructure_operators, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, infrastructure_operators, payer).

% External bodies (inspectors-general, academic disaster researchers, international peer-review missions) that periodically assess whether drill outcomes track real capability or have degraded into scripted performance. Their assessments are the primary external check on whether the competence reading actually holds.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, independent_auditors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that disaster response capability decays without practice: knowledge, coordination, and equipment familiarity atrophy between real events, so periodic live-scenario exercises re-validate and refresh actual operational competence across agencies and residents.
% TRANSFER_FUNCTION: Moves time, budget, and operational disruption from participating agencies and infrastructure operators into exercise design and execution; in return, moves validated capability and updated failure-mode knowledge back to the same agencies and to the population they protect. Under this reading the transfer is roughly reciprocal rather than extractive.
% ABSENT_VOICES: Frontline residents in low-drill-frequency peripheral districts rarely observe the exercises directly and have no seat in scenario design; if consulted they might argue drill locations and scenario types are concentrated where visibility to inspectors and funders is highest, not where actual risk is highest.
% DISAPPEARANCE_RATIONALE: If drills and inspections stopped, response agencies would retain formal training but lose the tacit, scenario-tested coordination skills that only live exercise produces; the next real disaster would surface coordination failures that current practice catches and corrects in advance. Infrastructure inspection lapses would similarly allow latent failure modes to accumulate undetected.
% FOUNDING_PROBLEM: Response agencies and infrastructure systems lose operational readiness between real disasters; formal training and static documentation do not reveal how personnel and systems actually behave under realistic, varied stress, so a mechanism was built to force periodic live re-validation.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic disaster-response researchers and international peer-review missions (outside the civil defense authority that runs and benefits from the program) corroborate that agencies exposed to varied, unscripted drills show measurably better real-incident coordination than agencies running only scripted or infrequent exercises, supporting the claim that the founding problem remains live and the mechanism still addresses it in jurisdictions that maintain scenario variation.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.18) and essentially flat over the interval because under this reading the coordination function has not degraded — participants and agencies continue to receive proportionate capability benefit for the time and disruption cost they bear. Theater ratio is authored low (0.15) and only slowly rising, reflecting a system that remains substantially functional rather than performative; the tiny upward drift acknowledges that even well-functioning systems accumulate some ceremonial residue over decades, without implying capture. Suppression is modest (0.22) because participation is compulsory for professional responders and regulated infrastructure operators, but this is closer to ordinary professional obligation than coercive extraction — accessibility_collapse (0.35) and resistance (0.2) are both authored low-to-moderate because credible alternatives to live practice (e.g., pure simulation, paper certification) are known and occasionally used, and little organized resistance exists to a system most participants regard as beneficial.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the competence reading, response agencies, drill participants, infrastructure operators, and the resident population are all authored as beneficiaries because the mechanism's core claim is that it produces reciprocal value — agencies pay in disruption and receive real capability; residents pay nothing directly and receive protection. There are no declared victims in this reading: the structural premise is that no one is net-extracted-from by a live-validated system. This is the sharpest point of divergence from the husk_reading sibling, where the same population would be recharacterized as bearing hidden risk from false assurance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (readiness decay between real disasters) is authored as still live, corroborated by independent academic and peer-review sources outside the civil defense authority itself — this blocks the mandatrophy read that would apply if the mechanism persisted only through institutional inertia after its function died. Because disappearance_verdict is world_rearranges and founding_problem_status is live, the mismatch-detection consumer should register these as consistent (no capture flag), which is the intended signature of a genuinely non-degraded coordination mechanism as opposed to its husk sibling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_indistinguishability,
    'From outside the system (e.g., before a real disaster tests it), can the competence_reading be empirically distinguished from the husk_reading using only observable drill performance metrics, or do both readings predict the same visible behavior until a real crisis occurs?',
    'Compare drill performance metrics (response time, coordination scores, improvisation under novel scenario injects) against post-incident after-action review outcomes across multiple real disasters; a persistent gap between drill scores and real-incident performance would falsify the competence reading in favor of the husk reading.',
    'If the readings are empirically indistinguishable absent a real disaster, the competence_reading''s classification as low-extraction rope rests partly on an untestable assumption between crisis events, and the corroboration claim in founding_problem_corroboration should be weighted accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_indistinguishability, empirical, 'Whether observable drill data can discriminate the competence reading from its husk sibling absent an actual disaster.').

omega_variable(
    scenario_variation_sufficiency,
    'Is the degree of scenario variation actually present in the drill program sufficient to force genuine improvisation, or has scenario variation itself narrowed over time into a small rotating set of expected inject types that participants have learned to recognize and pattern-match?',
    'Longitudinal content analysis of drill scenario design documents and post-drill participant surveys asking whether they recognized the scenario type before responding.',
    'If scenario variation has narrowed, the competence_reading''s core structural premise (novel failure signature recognition) weakens toward the hybrid_reading or husk_reading, and the low theater_ratio authored here would be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scenario_variation_sufficiency, empirical, 'Whether scenario variation remains genuinely novel or has degraded into a recognizable, gameable pattern.').

omega_variable(
    kernel_reading_selection_basis,
    'Which reading of the preparedness_transmission kernel is closest to true for a given jurisdiction at a given time, and what signal should an author or analyst use to select among competence_reading, husk_reading, and hybrid_reading when generating or evaluating a specific real-world drill program?',
    'Independent audit findings (from the independent_auditors stakeholder seat) comparing drill design documents, unannounced-element frequency, and real-incident after-action reports against the structural predictions of each reading.',
    'Selecting the wrong reading for a real jurisdiction misclassifies the constraint''s actual type — treating a hollowed-out husk system as a functioning rope would mask an emerging false-summit-style risk, while treating a genuinely functioning system as a husk would wrongly delegitimize effective preparedness institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'The evidentiary basis for choosing which kernel reading applies to a concrete preparedness system.').


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
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__competence_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__competence_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__competence_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__competence_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__competence_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(prep_su_t8, preparedness_transmission__competence_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(prep_su_t16, preparedness_transmission__competence_reading, suppression_requirement, 16, 0.21).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__competence_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement(prep_su_t32, preparedness_transmission__competence_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__competence_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language concept 'drills and inspections as preparedness transmission,' per the ε-invariance principle. competence_reading (this story) authors low, stable extraction and a functioning rope. husk_reading authors the same surface practices but with hollowed operational content, higher theater_ratio, and a materially different classification. hybrid_reading authors a stratified split between high physical-infrastructure competence and decayed civilian coordination knowledge, sitting structurally between the other two. Each carries its own ε and stakeholder structure; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
