% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation Sufficiency for Competence Occupation in High-Reliability Operations
 *   domain: safety/training/institutional
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, healthcare) operate
 *   under regulations that mandate competence certification through
 *   simulation-based training. This constraint instantiates the reading that
 *   simulation drills constitute sufficient exercise to occupy the competence
 *   kernel and prevent skill decay. The reading rests on the axiom that
 *   training compliance (hours, scenarios, check-box passage) is a valid
 *   surrogate for actual operator competence in live conditions. The
 *   competing reading—real_incident_necessity—holds that only authentic
 *   catastrophic conditions occupy the competence kernel; the
 *   hybrid_occupation reading holds that multiple mechanisms (simulation +
 *   refresher + line audits + incident analysis) are necessary without
 *   consensus on configuration. This constraint is ONE reading of these
 *   three. The claim is tangled_rope because the constraint coordinates
 *   genuine standardization while extracting compliance authority and
 *   training revenue.
 *
 * KEY AGENTS:
 *   - simulation_training_industry: Institutional beneficiary and agenda-setter; controls fidelity parameters and training protocols; collects licensing and certification revenue
 *   - regulatory_bodies: Beneficiary and secondary agenda-setter; mandate simulation-based training; capture objectifiable compliance metrics; delegate actual competence assessment
 *   - front_line_operators: Payers; identity-locked to the 'licensed competent' frame; bear costs of undetected skill decay; suppress doubts about simulation sufficiency
 *   - real_incident_advocates: Excluded; possess empirical evidence of the competence gap; kept out of the certification loop by the regulatory mandate
 *   - incident_investigators: Observers; see the pattern (training compliance + catastrophic failure); provide external corroboration but sit outside the feedback loop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.68).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.72).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation Sufficiency for Competence Occupation in High-Reliability Operations").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "safety/training/institutional").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '06ecb1e7-1ab4-419b-947b-599da1449cda').
narrative_ontology:cs_kernel_codification('06ecb1e7-1ab4-419b-947b-599da1449cda', formalized).
narrative_ontology:cs_authority_grounding('06ecb1e7-1ab4-419b-947b-599da1449cda', extraction).
narrative_ontology:cs_interpretation_layer_present('06ecb1e7-1ab4-419b-947b-599da1449cda').
narrative_ontology:cs_reading_relation('06ecb1e7-1ab4-419b-947b-599da1449cda', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('06ecb1e7-1ab4-419b-947b-599da1449cda', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('06ecb1e7-1ab4-419b-947b-599da1449cda', foundational, training_compliance_surrogate_for_competence).
narrative_ontology:cs_axiom_status(training_compliance_surrogate_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('06ecb1e7-1ab4-419b-947b-599da1449cda', training_compliance_surrogate_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('06ecb1e7-1ab4-419b-947b-599da1449cda', foundational, fidelity_optimization_convergence).
narrative_ontology:cs_axiom_status(fidelity_optimization_convergence, holdable).
narrative_ontology:cs_axiom_grounding('06ecb1e7-1ab4-419b-947b-599da1449cda', fidelity_optimization_convergence, instrumental).
narrative_ontology:cs_reference_frame('06ecb1e7-1ab4-419b-947b-599da1449cda', scalable_standardized_competence_certification).
narrative_ontology:cs_drift_state('06ecb1e7-1ab4-419b-947b-599da1449cda', contemporary_post_incident_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('06ecb1e7-1ab4-419b-947b-599da1449cda', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_training_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, regulatory_bodies).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, front_line_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, safety_culture).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, systems_operators).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, training_compliance_surrogate_for_competence).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, fidelity_optimization_convergence).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, incident_prevention_via_frequency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, builds, certifies, and sells simulation-based training platforms. Has financial incentive to establish simulation sufficiency as the standard for competence certification. Controls the fidelity parameters and training protocols that define 'adequate exercise.' Collects licensing fees, maintenance contracts, and upgrade revenue that depend on regulatory mandates requiring simulation-based training. Frames the relationship as solving a genuine coordination problem: standardized, reproducible, scalable training versus unpredictable, unsafe learning-from-incidents.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_training_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Mandate simulation-based training as the basis for competence certification and licensing. Benefit from the objectifiable compliance metric (training hours, scenario completion, check-box audits) versus the harder task of measuring actual operator competence in the field. Can audit training records; cannot easily audit skill maintenance in live operations without incident data. Delegates competence assessment to the simulation industry while retaining nominal oversight.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_bodies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, regulatory_bodies, agenda_setter).

% Must complete and pass mandated simulation drills to maintain licensure and keep their positions. Experience simulation as sanitized, predictable, often low-consequence exercises that differ structurally from the conditions operators encounter in live operations (fatigue, communication breakdowns, compressed time, cascading surprises, real stakes). Carry the identity of 'licensed competent operator' that is decoupled from their felt sense of readiness. Cannot exit without abandoning their career. Internalize the simulation-compliance frame as sufficient even when they privately doubt it. Bear the cost of skill decay undetected until an actual incident exposes the gap.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, front_line_operators, payer,
    moderate, biographical, identity_locked, local).

% Safety researchers, some front-line operators, and systems-thinking disciplines that argue competence occupation requires exposure to authentic incident conditions. Would argue for hybrid or incident-grounded approaches. Excluded from the simulation-sufficiency framing by the regulatory mandate; their empirical evidence and professional judgment are treated as secondary to compliance metrics. Would contribute substantially different framings if seated in the competence-assessment conversation.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, real_incident_advocates, excluded,
    organized, biographical, constrained, global).

% Operating organizations (airlines, nuclear, hospitals) that manage fleets of licensed operators. Benefit from the reduction in training cost and scheduling complexity that simulation provides (no need to manufacture incidents, operators can be trained on standardized curricula without waiting for real events). Benefit from the liability shield of being able to demonstrate 'operators completed mandated training.' Bear hidden costs when undetected skill decay contributes to incidents, but the causal link is deniable ('operator passed all required checks').
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, systems_operators, beneficiary,
    powerful, generational, mobile, national).

% Analyze accidents and near-misses to understand failure chains. Observe the pattern: competence on the training record, catastrophic performance failure in real conditions. Provide external corroboration of the competence-occupation gap but sit outside the regulatory and training certification loop. Their findings are treated as accident explanation, not as feedback to the training system. Analytical seat: can see the full structure but cannot change the constraint's enforcement.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, incident_investigators, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_training_industry).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of standardizing competence assessment across a distributed population of operators, enabling scalable licensing and cross-organizational credentialing without requiring each organization to run its own incident-based training curriculum.
% TRANSFER_FUNCTION: Moves regulatory authority and training revenue from incident-response and field-based learning systems to the simulation industry, which designs and certifies the training that becomes the legal basis for operator licensing. Operators invest time and cognitive effort in simulations; the industry and regulators capture the standardization premium and audit authority.
% ABSENT_VOICES: Real-incident-grounded safety researchers, front-line operators who have survived actual incidents and know the gap between simulation and reality, and safety culture advocates are structurally absent from the competence-definition conversation. Their evidence and expertise are available but not seated in the regulatory authority structure. If present, they would argue for hybrid occupation mechanisms and would flag the performance gap as predictable.
% DISAPPEARANCE_RATIONALE: If the simulation-sufficiency mandate vanished, regulatory bodies would scramble to redefine competence assessment, the simulation industry would lose its guaranteed market, and operator training would fragment back into hybrid models (simulation + refresher + incident analysis). The constraint's disappearance would force a return to the competence-occupation problem unsolved, or adoption of one of the sibling readings. Organizations would face higher training costs and uncertainty about skill maintenance methods.
% FOUNDING_PROBLEM: Early approaches to competence maintenance relied on rare, catastrophic incidents as the primary teacher. This produced: long periods of untested operators between incidents, difficulty scaling training to new organizations, ethical and safety hazards of learning-by-real-incident, and no systematic way to maintain competence in domains where incidents are rare (catastrophic risk systems). Simulation was proposed to solve the scale and ethics problem: standardized, repeatable, safe rehearsal of critical scenarios.
% FOUNDING_PROBLEM_CORROBORATION: The simulation industry and regulatory bodies attest the problem is still live, citing the need for scalable, safe, standardized training. Real-incident advocates and systems researchers from outside the training industry attest the founding problem was partially solved (scale and ethics are addressed) but a new problem was created: simulation-sufficiency doctrine prevents detection and correction of the skill-decay gap. Post-incident investigations and aviation/nuclear safety audits provide external corroboration of the competence gap; this evidence sits outside the training-certification feedback loop.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 because the simulation industry and regulatory bodies capture standardization premium and training revenue while front-line operators and safety culture bear hidden costs (undetected skill decay, eroded incident-reporting culture). Suppression is 0.72 because enforcement depends on maintaining the simulation-sufficiency frame against empirical evidence and operator doubts; real-incident advocates are excluded from the conversation; operators are identity-locked into compliance even when they doubt sufficiency. Theater ratio reaches 0.58 because the constraint increasingly performs training completion rather than measuring competence—the gap widens as scenarios proliferate and fidelity optima become contested, but compliance metrics remain the regulatory standard. Accessibility_collapse and resistance show the leveled picture: individual-level alternatives are constrained (operators cannot exit without career loss), class-level resistance is substantial but diffuse (operators share doubts but cannot coordinate around them), structural-level alternatives are closing as the mandate spreads. Measurement series show extraction and theater rising over time as the simulation industry expands its market and operators internalize the compliance frame.
 *
 * PERSPECTIVAL GAP:
 *   From the simulation industry and regulatory seats: this is genuine coordination—we solved the scale and ethics problem of learning-by-incident and enabled standardized, reproducible competence assessment. From the front-line operator and safety-culture seats: this is enforced compliance—we pass the training because we must keep our jobs, but we know the simulation differs structurally from real conditions and we report fewer safety concerns because 'the training says we're competent.' From the incident-investigator seat: the divergence is visible—operators passed all checks and catastrophically failed because competence occupation requires authentic incident exposure that simulation cannot provide. The engine's per-seat classification should surface these divergences without reconciling them.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation_training_industry: d near 0.0 (full beneficiary—collects revenue, sets standards, gains institutional authority). Regulatory_bodies: d near 0.2 (beneficiary—capture compliance metrics, audit authority, reduced regulatory burden). Front_line_operators: d near 0.85 (target—must complete training, identity-locked, bear skill-decay costs, suppressed doubts). Real_incident_advocates: d near 1.0 (target—excluded from the decision loop, empirical evidence suppressed). Systems_operators: d near 0.15 (beneficiary—training cost and scheduling simplification, liability shield). Incident_investigators: d near 0.5 (symmetric—required to investigate failures, but findings sit outside the feedback loop that could change the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (safe, scalable, standardized competence training) was genuine; the founding_problem_status is contested because the problem is PARTIALLY solved (scale and ethics are addressed) but a new problem is CREATED (skill-decay detection and correction are disabled). The constraint prevents mandatrophy resolution because the regulatory mandate treats training compliance as sufficient, which locks out the adaptive loop (incident investigation → competence protocol refinement → re-training) that would otherwise correct the occupational gap. The theater_ratio increase (0.38→0.58 over the interval) indicates the constraint's function is shifting toward performance of compliance and away from actual competence maintenance. The divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges flags this as a zombie constraint: the founding problem is incompletely solved but the constraint persists and is actively defended by the benefiting parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_authenticity_bound,
    'What is the empirical lower bound on the gap between simulation fidelity and real-incident conditions? Can this gap be closed by increasing simulator cost and complexity, or is it structurally irreducible?',
    'Longitudinal comparison of operator performance in simulators vs. real incidents across multiple domains (aviation, nuclear, healthcare); process tracing of operator decision-making under authentic stress, time pressure, and cascading failures.',
    'If the gap is structurally irreducible even at high fidelity, simulation-sufficiency is falsified and hybrid_occupation or real_incident_necessity readings become mandatory. If the gap closes with fidelity improvement, simulation-sufficiency remains holdable and the constraint''s classification depends on what fidelity level is cost-effective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_authenticity_bound, empirical, 'Whether simulation can achieve sufficient authenticity through optimization or whether the gap is fundamental.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the measured suppression in front_line_operators structural (regulatory enforcement, job loss threat) or internalized (identity-fusion with ''licensed competent,'' internalized doubt about self-knowledge)?',
    'Post-regulatory change observation: if operators show substantial behavioral change (e.g., incident reporting increase, expressed doubt about training) after removal of the simulation-sufficiency mandate, the suppression is partly internalized. If behavior remains unchanged, suppression is structural.',
    'If internalized, the constraint carries the suppression with it in operator cognition even after formal regulatory change—the competence-occupation problem persists at the individual psychological level. If structural, regulatory replacement with an alternative reading could shift operator behavior quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Suppression mechanism in identity-locked operators.').

omega_variable(
    regulatory_capture_degree,
    'To what extent are regulatory bodies genuinely autonomous in competence-certification authority, versus captured by the simulation industry through expertise dependence and revolving-door hiring?',
    'Institutional history of regulatory competence-assessment rules; tracing of personnel movement between industry and regulatory seats; comparison of competence standards in regulated vs. non-regulated jurisdictions.',
    'High capture would establish the beneficiary structure (industry + regulatory body as unified seat) and suggest the constraint is extractive snare rather than coordination. Low capture would support the tangled_rope classification (genuine coordination with asymmetric extraction as a side effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_degree, empirical, 'Degree of simulation industry capture of regulatory authority.').

omega_variable(
    real_incident_frequency_baseline,
    'What is the baseline incident frequency in the domain if simulation-based training is abandoned and replaced with real-incident-grounded learning?',
    'Historical data from pre-simulation era; natural experiments from domains without mandated simulation; economic modeling of incident rates under different training regimes.',
    'If baseline frequency is high enough to provide adequate learning opportunities without catastrophic risk, real_incident_necessity becomes more viable. If baseline frequency is too low to support learning before catastrophic failure, simulation-sufficiency (or hybrid_occupation) remains necessary even if imperfect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(real_incident_frequency_baseline, empirical, 'Whether real incidents alone provide sufficient learning opportunities.').

omega_variable(
    committer_axiom_empirical_challenge,
    'Has the empirical premise underlying the training_compliance_surrogate axiom been substantially challenged by systematic evidence since regulatory adoption?',
    'Systematic review of post-incident investigations, safety audits, and competence studies showing correlation (or lack thereof) between training compliance and actual operator performance in real conditions.',
    'If the axiom has been empirically challenged, it approaches overridden status and the reading''s foundation weakens. The engine may compute foreclosure pressure on simulation_sufficiency from the empirical evidence. If the axiom remains empirically unchallenged, the reading remains holdable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_axiom_empirical_challenge, empirical, 'Empirical status of the training_compliance_surrogate foundational axiom.').

omega_variable(
    theater_ratio_measurement_ambiguity,
    'What fraction of simulation activity represents genuine competence rehearsal vs. performative compliance (checking boxes, clock-running, CYA training)?',
    'Embedded observation of simulation sessions; operator interviews about simulation fidelity and relevance; content analysis of incident reports tracing failure modes back to simulation-neglected scenarios.',
    'If theater ratio approaches 0.7+, the constraint is increasingly performing compliance rather than occupying competence; piton classification becomes likely despite the claimed tangled_rope type. If theater ratio stays below 0.5, the constraint retains functional coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_ambiguity, empirical, 'Genuine vs. performative share of simulation activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(comp_tr_t0, projected).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__simulation_sufficiency, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__simulation_sufficiency, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__simulation_sufficiency, theater_ratio, 25, 0.55).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__simulation_sufficiency, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(comp_be_t0, projected).
narrative_ontology:measurement(comp_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_occupation__simulation_sufficiency, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t25, competence_occupation__simulation_sufficiency, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t40, competence_occupation__simulation_sufficiency, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comp_su_t0, projected).
narrative_ontology:measurement(comp_su_t5, competence_occupation__simulation_sufficiency, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_occupation__simulation_sufficiency, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_occupation__simulation_sufficiency, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t25, competence_occupation__simulation_sufficiency, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t40, competence_occupation__simulation_sufficiency, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(comp_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(comp_grid_01, competence_occupation__simulation_sufficiency, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(comp_grid_02, competence_occupation__simulation_sufficiency, accessibility_collapse(class), 40, 0.54).
narrative_ontology:measurement(comp_grid_03, competence_occupation__simulation_sufficiency, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(comp_grid_04, competence_occupation__simulation_sufficiency, accessibility_collapse(individual), 40, 0.42).
narrative_ontology:measurement(comp_grid_05, competence_occupation__simulation_sufficiency, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(comp_grid_06, competence_occupation__simulation_sufficiency, accessibility_collapse(organizational), 40, 0.58).
narrative_ontology:measurement(comp_grid_07, competence_occupation__simulation_sufficiency, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(comp_grid_08, competence_occupation__simulation_sufficiency, accessibility_collapse(structural), 40, 0.65).
narrative_ontology:measurement(comp_grid_09, competence_occupation__simulation_sufficiency, resistance(class), 0, 0.65).
narrative_ontology:measurement(comp_grid_10, competence_occupation__simulation_sufficiency, resistance(class), 40, 0.61).
narrative_ontology:measurement(comp_grid_11, competence_occupation__simulation_sufficiency, resistance(individual), 0, 0.62).
narrative_ontology:measurement(comp_grid_12, competence_occupation__simulation_sufficiency, resistance(individual), 40, 0.58).
narrative_ontology:measurement(comp_grid_13, competence_occupation__simulation_sufficiency, resistance(organizational), 0, 0.48).
narrative_ontology:measurement(comp_grid_14, competence_occupation__simulation_sufficiency, resistance(organizational), 40, 0.52).
narrative_ontology:measurement(comp_grid_15, competence_occupation__simulation_sufficiency, resistance(structural), 0, 0.58).
narrative_ontology:measurement(comp_grid_16, competence_occupation__simulation_sufficiency, resistance(structural), 40, 0.54).
narrative_ontology:measurement(comp_grid_17, competence_occupation__simulation_sufficiency, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(comp_grid_18, competence_occupation__simulation_sufficiency, stakes_inflation(class), 40, 0.64).
narrative_ontology:measurement(comp_grid_19, competence_occupation__simulation_sufficiency, stakes_inflation(individual), 0, 0.62).
narrative_ontology:measurement(comp_grid_20, competence_occupation__simulation_sufficiency, stakes_inflation(individual), 40, 0.68).
narrative_ontology:measurement(comp_grid_21, competence_occupation__simulation_sufficiency, stakes_inflation(organizational), 0, 0.45).
narrative_ontology:measurement(comp_grid_22, competence_occupation__simulation_sufficiency, stakes_inflation(organizational), 40, 0.48).
narrative_ontology:measurement(comp_grid_23, competence_occupation__simulation_sufficiency, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(comp_grid_24, competence_occupation__simulation_sufficiency, stakes_inflation(structural), 40, 0.55).
narrative_ontology:measurement(comp_grid_25, competence_occupation__simulation_sufficiency, suppression(class), 0, 0.65).
narrative_ontology:measurement(comp_grid_26, competence_occupation__simulation_sufficiency, suppression(class), 40, 0.72).
narrative_ontology:measurement(comp_grid_27, competence_occupation__simulation_sufficiency, suppression(individual), 0, 0.68).
narrative_ontology:measurement(comp_grid_28, competence_occupation__simulation_sufficiency, suppression(individual), 40, 0.75).
narrative_ontology:measurement(comp_grid_29, competence_occupation__simulation_sufficiency, suppression(organizational), 0, 0.42).
narrative_ontology:measurement(comp_grid_30, competence_occupation__simulation_sufficiency, suppression(organizational), 40, 0.45).
narrative_ontology:measurement(comp_grid_31, competence_occupation__simulation_sufficiency, suppression(structural), 0, 0.58).
narrative_ontology:measurement(comp_grid_32, competence_occupation__simulation_sufficiency, suppression(structural), 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.12).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel is contested across three readings, each instantiating a different constraint with different ε values and beneficiary structures. This reading (simulation_sufficiency) claims training compliance is sufficient and treats skill decay as optimizable through frequency/fidelity. The real_incident_necessity reading claims only catastrophic incidents occupy the competence kernel. The hybrid_occupation reading claims multiple mechanisms are necessary without consensus. All three readings share the kernel (competence_occupation as the core commitment) but diverge on what constitutes adequate occupation. They are linked via network.affects_constraints; see the committer frame (kernel_context, cs_structure.reading_relations, cs_structure.axioms) for the structural relationships and foundational axioms that distinguish them. Do not attempt to merge these three stories into one—they are ε-invariant, have different beneficiary/victim structures, and would produce incompatible classification results if forced into a single constraint. The kernel contest is modeled as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
