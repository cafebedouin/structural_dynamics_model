% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Multi-Mechanism Competence Occupation Regime (Simulation + Refresher + Procedural Reinforcement + Line Audit)
 *   domain: safety/organizational/high_reliability
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear, aviation, chemical processing)
 *   mandate that operator competence be maintained through simultaneous
 *   simulation training, periodic refreshers, procedural reinforcement
 *   drills, and line audits. No mechanism alone is trusted, and no research
 *   consensus exists on the optimal mix or dosage of mechanisms. This story
 *   instantiates the 'hybrid_occupation' reading of the competence-occupation
 *   kernel: the position that occupying the competence kernel legitimately
 *   REQUIRES layering multiple imperfect, partially redundant mechanisms
 *   because none is individually sufficient and their combination is the best
 *   available proxy for real competence. This reading is distinct from, and
 *   in institutional tension with, two sibling readings authored as separate
 *   constraints: simulation_sufficiency (simulation alone suffices) and
 *   real_incident_necessity (only genuine catastrophic exposure counts). The
 *   three readings compete for institutional resources and regulatory
 *   language; this file does not adjudicate between them, only characterizes
 *   the hybrid position's own structure.
 *
 * KEY AGENTS:
 *   - senior_safety_management: agenda_setter, designs and can alter the mechanism mix
 *   - training_vendor_consortium: beneficiary, profits from mechanism proliferation and non-convergence
 *   - regulatory_certification_bodies: beneficiary/agenda_setter, gains discretionary authority from unresolved configuration questions
 *   - frontline_operators: payer, bears the full compliance burden with no exit
 *   - shift_supervisors: payer/agenda_setter, enforces downward while absorbing pressure from above
 *   - smaller_operating_sites: payer, cannot absorb the staffing cost of parallel mechanisms
 *   - safety_researchers: observer, produces the inconclusive evidence base that perpetuates the regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.46).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.52).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.46).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Multi-Mechanism Competence Occupation Regime (Simulation + Refresher + Procedural Reinforcement + Line Audit)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "safety/organizational/high_reliability").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '67a8a86e-132d-4452-a1c9-b12b66bec9a1').
narrative_ontology:cs_kernel_codification('67a8a86e-132d-4452-a1c9-b12b66bec9a1', distributed).
narrative_ontology:cs_authority_grounding('67a8a86e-132d-4452-a1c9-b12b66bec9a1', expertise).
narrative_ontology:cs_interpretation_layer_present('67a8a86e-132d-4452-a1c9-b12b66bec9a1').
narrative_ontology:cs_reading_relation('67a8a86e-132d-4452-a1c9-b12b66bec9a1', competence_occupation__simulation_sufficiency, influences).
narrative_ontology:cs_reading_relation('67a8a86e-132d-4452-a1c9-b12b66bec9a1', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('67a8a86e-132d-4452-a1c9-b12b66bec9a1', foundational, no_single_mechanism_is_epistemically_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_is_epistemically_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('67a8a86e-132d-4452-a1c9-b12b66bec9a1', no_single_mechanism_is_epistemically_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('67a8a86e-132d-4452-a1c9-b12b66bec9a1', secondary, mechanism_redundancy_reduces_blind_spot_risk).
narrative_ontology:cs_axiom_status(mechanism_redundancy_reduces_blind_spot_risk, holdable).
narrative_ontology:cs_axiom_grounding('67a8a86e-132d-4452-a1c9-b12b66bec9a1', mechanism_redundancy_reduces_blind_spot_risk, instrumental).
narrative_ontology:cs_reference_frame('67a8a86e-132d-4452-a1c9-b12b66bec9a1', post_incident_layered_training_consensus).
narrative_ontology:cs_drift_state('67a8a86e-132d-4452-a1c9-b12b66bec9a1', contemporary_vendor_saturated_compliance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('67a8a86e-132d-4452-a1c9-b12b66bec9a1', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_vendor_consortium).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, regulatory_certification_bodies).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, senior_safety_management).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, shift_supervisors).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, smaller_operating_sites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates the multi-mechanism competence regime, deciding the mix of simulation hours, refresher cadence, procedural drills, and line audit frequency. Can adjust the configuration at will and points to the absence of consensus on optimal design as justification for layering mechanisms rather than choosing among them. Bears none of the compliance burden personally.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, senior_safety_management, agenda_setter,
    institutional, generational, arbitrage, national).

% Sells simulation platforms, refresher curricula, and audit software. Has a direct financial stake in the regime never converging on a single sufficient mechanism, since consolidation would shrink the addressable market. Can exit into adjacent training markets if this one closes; faces no personal exposure to incident consequences.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_vendor_consortium, beneficiary,
    organized, biographical, arbitrage, national).

% Certifies compliance with the multi-mechanism standard and derives institutional legitimacy from having a defensible, elaborate certification apparatus. Benefits from the absence of consensus because it justifies continued rulemaking authority; a settled optimal configuration would reduce the certifier's discretionary power.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulatory_certification_bodies, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, regulatory_certification_bodies, agenda_setter).

% Must complete simulation sessions, refresher modules, procedural walkthroughs, and submit to line audits — often overlapping and redundant — on top of full operational duties. Cannot decline without losing certification and employment; cannot choose which mechanism to prioritize since all are mandatory simultaneously. Experiences the regime as an unending compliance treadmill rather than skill development.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    powerless, immediate, trapped, local).

% Administers the line audits and enforces refresher compliance on their crews while also being subject to the same requirements from above. Absorbs scheduling conflict between production demands and training mandates, and is blamed locally when audit findings surface, without authority to change the mechanism mix.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, shift_supervisors, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, shift_supervisors, agenda_setter).

% Lacks the staffing depth to run parallel mechanisms without disrupting operations; must either understaff shifts during training windows or fall behind on compliance, both of which carry regulatory risk. Cannot negotiate a leaner configuration because the standard is uniform across site size.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, smaller_operating_sites, payer,
    moderate, biographical, constrained, regional).

% Studies skill decay curves, audit outcome data, and simulation transfer rates across sites, publishing findings that variously support and undercut each mechanism's marginal contribution. Their inability to reach consensus is itself part of what perpetuates the multi-mechanism mandate, since no single study forecloses the others.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, training_vendor_consortium).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuinely hard problem: competence in high-consequence, low-frequency-event domains decays without exercise, and no single observable (simulation score, audit pass rate, procedural recall) reliably predicts real-world performance, so multiple imperfect proxies are combined to reduce blind spots.
% TRANSFER_FUNCTION: Moves operator time, cognitive bandwidth, and site staffing capacity into a stack of training and audit mechanisms, and moves revenue and institutional discretion toward vendors and certifiers who administer that stack.
% ABSENT_VOICES: Frontline operators who experience the mechanisms as redundant rather than complementary are rarely consulted on configuration design; their felt burden is treated as a cost of doing business rather than evidence the configuration is inefficient. Smaller sites lack a forum to argue for a scaled configuration.
% DISAPPEARANCE_RATIONALE: Safety management and certifiers would say the world rearranges catastrophically — competence would decay unchecked and incident rates would rise. Frontline operators and smaller sites would say a leaner, better-targeted configuration would leave real competence outcomes roughly unchanged while freeing substantial operational capacity; researchers cannot adjudicate the dispute because the optimal configuration itself is unresolved.
% FOUNDING_PROBLEM: Single-mechanism training regimes (simulation-only, or refresher-only) were repeatedly shown insufficient after incidents where certified-competent operators still failed under real conditions, motivating a layered, redundant approach to competence maintenance.
% FOUNDING_PROBLEM_CORROBORATION: Independent incident investigation boards outside the training-vendor and certifier ecosystem attest the underlying decay-and-blind-spot problem remains live and genuinely unsolved. However, the same outside investigators have also found no evidence that the specific four-mechanism configuration outperforms leaner alternatives — the persistence of THIS configuration, as opposed to a resolved question of what competence maintenance requires, is corroborated mainly by the beneficiary parties themselves.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, contested).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).
:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46) and rising: the layering itself is defensible given genuine decay/blind-spot risk, but the trend line shows steady accumulation consistent with mechanisms being added rather than optimized or pruned — additive rather than substitutive institutional behavior. Theater ratio at 0.4 reflects that a meaningful share of activity (particularly redundant audits layered atop already-audited simulation performance) functions more as institutional liability defense than as skill-building. Suppression (0.52) is moderate: operators cannot opt out of any mechanism without losing certification, but the suppression is procedural rather than punitive — no single actor is coercing compliance for personal gain, it is the joint output of certifier requirements and employer liability management. Accessibility collapse (0.6) reflects that leaner alternative configurations are structurally foreclosed by certification standards, not by superior evidence for the current configuration.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior safety management and certification bodies sit near the beneficiary end: they set the mechanism mix, gain institutional legitimacy and discretionary authority from its complexity, and bear no personal compliance cost. The training vendor consortium is a clear beneficiary with arbitrage-grade exit into adjacent markets if any single mechanism is discredited. Frontline operators and smaller sites sit near the target end: trapped or constrained exit, immediate time horizon, and the full weight of compliance falls on them without proportional say in configuration design. Shift supervisors occupy a genuinely dual position — enforcing the regime downward while absorbing its costs, which the secondary_role field captures rather than forcing a single classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (single-mechanism regimes failing under real conditions) is corroborated as still live by independent incident boards — this prevents the constraint from being dismissed as pure mandatrophy. But the SPECIFIC four-mechanism configuration, as opposed to some leaner sufficient configuration, is corroborated mainly by the parties who profit from its complexity. This is the seat divergence the tangled_rope classification is built to hold: real coordination function (decay is real, blind spots are real) coexists with asymmetric extraction (the configuration's specific shape serves vendor and certifier interests more than it serves demonstrated competence outcomes) — reducing this to either pure Rope or pure Snare would erase one half of the true structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_configuration_optimality_ambiguity,
    'Is the specific four-mechanism configuration (simulation + refresher + procedural reinforcement + line audit) actually superior to a leaner two- or three-mechanism configuration, or is its complexity sustained by vendor and certifier interest in non-convergence rather than by evidence?',
    'Controlled cross-site comparison of configurations with matched incident-rate and near-miss outcome tracking over a multi-year horizon, run by parties without financial stake in any mechanism vendor.',
    'If a leaner configuration performs equivalently, the current regime''s additional layers are excess extraction dressed as coordination; if the full stack is genuinely necessary, the extractiveness score should be revised downward and the regime reclassified closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_configuration_optimality_ambiguity, empirical, 'Whether the specific multi-mechanism configuration is evidence-driven or vendor/certifier-driven.').

omega_variable(
    kernel_reading_foreclosure_scope,
    'Does adopting the hybrid_occupation reading as institutional policy structurally foreclose the simulation_sufficiency reading within the same regulatory framework, or can they coexist as competing certification pathways?',
    'Comparative regulatory analysis of jurisdictions that permit simulation-only certification alongside jurisdictions mandating the full hybrid stack, to determine whether both pathways can be legally simultaneous or are mutually exclusive within one certifying body''s rules.',
    'If mutually exclusive, this reading''s adoption forecloses simulation_sufficiency as a live regulatory option in that jurisdiction, strengthening the certifier''s discretionary authority; if coexistent, the readings merely compete for institutional preference without one displacing the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_scope, conceptual, 'Whether hybrid_occupation and simulation_sufficiency can coexist within one regulatory framework or are mutually exclusive.').

omega_variable(
    researcher_consensus_incentive_structure,
    'Does the safety-research community''s persistent inability to reach consensus on optimal configuration reflect genuine irreducible measurement difficulty, or is it partly sustained by research funding streams tied to vendors and certifiers who benefit from non-convergence?',
    'Funding-source disclosure audit of published skill-decay and audit-outcome studies, cross-referenced against which studies favor configuration expansion versus consolidation.',
    'If funding-tied, the ''no consensus'' framing itself is partially an artifact of the extraction structure rather than a neutral epistemic fact, which would raise the effective extractiveness and suppression scores.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(researcher_consensus_incentive_structure, empirical, 'Whether research non-consensus is genuine or partly funding-incentivized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__hybrid_occupation, theater_ratio, 4, 0.26).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__hybrid_occupation, theater_ratio, 8, 0.3).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__hybrid_occupation, theater_ratio, 12, 0.33).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__hybrid_occupation, theater_ratio, 16, 0.36).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.38).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__hybrid_occupation, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t4, competence_occupation__hybrid_occupation, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(comp_be_t8, competence_occupation__hybrid_occupation, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(comp_be_t12, competence_occupation__hybrid_occupation, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(comp_be_t16, competence_occupation__hybrid_occupation, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(comp_be_t24, competence_occupation__hybrid_occupation, base_extractiveness, 24, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t4, competence_occupation__hybrid_occupation, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(comp_su_t8, competence_occupation__hybrid_occupation, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(comp_su_t12, competence_occupation__hybrid_occupation, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(comp_su_t16, competence_occupation__hybrid_occupation, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(comp_su_t24, competence_occupation__hybrid_occupation, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__hybrid_occupation, 0.12).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, real_incident_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'competence occupation' under the ε-invariance principle. hybrid_occupation, simulation_sufficiency, and real_incident_necessity each claim a different sufficient condition for occupying the same underlying kernel, and each has a distinct ε, distinct stakeholders, and distinct classification. They are linked bidirectionally via affects_constraints because institutional adoption of any one reading structurally changes resource availability and legitimacy conditions for the other two — e.g., regulatory endorsement of the hybrid stack reduces the political viability of simulation-only certification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
