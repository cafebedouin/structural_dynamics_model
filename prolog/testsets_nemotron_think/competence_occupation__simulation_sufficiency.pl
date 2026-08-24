% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation Sufficiency for Competence Kernel Occupation
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, healthcare, rail)
 *   mandate recurring simulation-based drills as the primary evidence that
 *   certified personnel maintain competence. The reading claims simulation is
 *   sufficient — that artificial exercise at adequate fidelity and frequency
 *   occupies the competence kernel and prevents skill decay. This claim
 *   structures a global training compliance regime: regulators mandate hours,
 *   auditors verify completion, vendors supply platforms, operators pay. The
 *   simulation industry has become the primary beneficiary, capturing a
 *   growing share of training budgets while the compliance metric
 *   (hours/fidelity) displaces actual competence assessment. The constraint
 *   presents as coordination (a shared solution to the observability problem)
 *   but operates with asymmetric extraction — the industry and compliance
 *   apparatus benefit materially while frontline operators and operating
 *   organizations bear costs with uncertain competence returns. The contested
 *   kernel is 'what occupies the competence kernel'; this reading
 *   instantiates the simulation-sufficiency position.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.68).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.55).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation Sufficiency for Competence Kernel Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '49d10bca-99f0-4ff6-9333-8af293b13804').
narrative_ontology:cs_kernel_codification('49d10bca-99f0-4ff6-9333-8af293b13804', formalized).
narrative_ontology:cs_authority_grounding('49d10bca-99f0-4ff6-9333-8af293b13804', extraction).
narrative_ontology:cs_interpretation_layer_present('49d10bca-99f0-4ff6-9333-8af293b13804').
narrative_ontology:cs_reading_relation('49d10bca-99f0-4ff6-9333-8af293b13804', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_reading_relation('49d10bca-99f0-4ff6-9333-8af293b13804', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('49d10bca-99f0-4ff6-9333-8af293b13804', foundational, simulation_fidelity_sufficiency_for_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_sufficiency_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('49d10bca-99f0-4ff6-9333-8af293b13804', simulation_fidelity_sufficiency_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('49d10bca-99f0-4ff6-9333-8af293b13804', secondary, competence_kernel_unitary_occupiability).
narrative_ontology:cs_axiom_status(competence_kernel_unitary_occupiability, holdable).
narrative_ontology:cs_axiom_grounding('49d10bca-99f0-4ff6-9333-8af293b13804', competence_kernel_unitary_occupiability, conventional).
narrative_ontology:cs_reference_frame('49d10bca-99f0-4ff6-9333-8af293b13804', post_tmi_crm_mandate_framework).
narrative_ontology:cs_drift_state('49d10bca-99f0-4ff6-9333-8af293b13804', contemporary_fidelity_escalation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('49d10bca-99f0-4ff6-9333-8af293b13804', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_compliance_auditors).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, regulatory_bodies_mandating_simulation).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, operating_organizations).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, taxpayers_public_funding_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, simulation_fidelity_equivalence_doctrine).
narrative_ontology:constraint_vindicates(competence_occupation__simulation_sufficiency, competence_kernel_occupiability_via_artificial_exercise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Vendors of high-fidelity simulators, scenario libraries, and training management platforms. They capture revenue from mandatory recurring training cycles, certification renewals, and fidelity upgrade mandates. Their market exists because regulation and industry standards treat simulation completion as the primary evidence of competence maintenance. They lobby for higher fidelity requirements and more frequent mandated sessions.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_industry, beneficiary,
    organized, generational, arbitrage, global).

% Internal and external auditors who verify training compliance. Their professional standing and institutional mandate depend on the simulation record being the authoritative evidence of competence. They define what counts as a valid drill, set scenario requirements, and certify completion. They benefit from the administrative burden of compliance tracking.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, training_compliance_auditors, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, training_compliance_auditors, agenda_setter).

% Aviation authorities, nuclear regulators, healthcare accreditation bodies. They mandate simulation hours and fidelity standards as the measurable proxy for competence assurance. This gives them a legible, auditable compliance lever. They benefit from the appearance of rigorous oversight without needing to assess actual operational performance.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_bodies_mandating_simulation, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, regulatory_bodies_mandating_simulation, beneficiary).

% Pilots, control room operators, surgeons, dispatchers. They must complete mandated simulation hours to retain certification and employment. Some genuinely value the practice; others experience it as performative box-checking that displaces rest or line experience. Their professional identity is fused with certification — leaving means losing license and career. They bear the time, fatigue, and opportunity cost; they also capture some skill maintenance benefit.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, frontline_operators, beneficiary).

% Airlines, nuclear plants, hospitals, rail operators. They fund simulation facilities, instructor cadres, and lost operational capacity during training. They comply because regulators require it and insurers price to it. They cannot easily exit — the mandate is a condition of operating license. They bear the direct financial cost and the indirect cost of workforce availability.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, operating_organizations, payer,
    powerful, biographical, constrained, national).

% Public funds subsidize simulation infrastructure for military, civil nuclear, public healthcare, and transport. They pay for the capital and operating costs of training centers. They have no direct exit — the mandate is set by regulators they fund but do not control. They bear diffuse cost with no visibility into whether competence is actually maintained.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, taxpayers_public_funding_bodies, payer,
    organized, generational, trapped, national).

% Operators and analysts who argue that only real events (near-misses, minor incidents, catastrophic accidents) generate the psychological and physiological conditions that truly test and maintain competence. They are structurally excluded because their evidence is anecdotal, uncontrolled, and ethically fraught — no regulator can mandate accidents. Their voice appears in post-accident inquiries but not in training standard-setting.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, real_incident_advocates, excluded,
    powerless, biographical, trapped, local).

% Human factors researchers, some training directors, and safety scientists who argue competence requires a mix: simulation, line audits, procedural refreshers, peer debrief, and controlled real-world exposure. They are marginalized in standard-setting because their model lacks a single compliance metric — it cannot be audited as cleanly as simulation hours.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, hybrid_occupation_proponents, excluded,
    moderate, biographical, constrained, global).

% Academic and applied researchers studying skill decay, transfer of training, and competence assessment. They produce evidence on simulation fidelity thresholds, decay curves, and the limits of artificial exercise. Their findings are cited selectively — high-fidelity transfer studies are amplified; decay-persistence and transfer-gap findings are downplayed in regulatory justification.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, competence_research_community, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, auditable, repeatable mechanism for organizations and regulators to verify that personnel have recently exercised critical skills without requiring catastrophic events. Solves the observability problem: competence is latent; simulation makes it visible and timestamped.
% TRANSFER_FUNCTION: Moves budget, workforce hours, and regulatory compliance burden from operating organizations and public funds to the simulation industry and compliance apparatus. Moves certification risk from regulators (who can point to completed hours) to operators (who must produce them). Moves psychological load to frontline personnel who must perform in artificial scenarios.
% ABSENT_VOICES: Real-incident advocates (operators who have lived through events and argue nothing else replicates the stress) and hybrid-occupation proponents (researchers and practitioners who see simulation as necessary but insufficient) are excluded from standard-setting bodies. They are absent because their evidence resists the single-metric audit logic that makes simulation governance tractable.
% DISAPPEARANCE_RATIONALE: If simulation mandates vanished overnight, regulators would lose their primary compliance lever and would need to develop performance-based or incident-informed assessment. Operating organizations would redirect training budgets — some to line checks, some to procedural review, some to controlled operational exposure. The simulation industry would contract sharply. Frontline operators would lose a structured practice venue but gain operational time. The competence assurance regime would fundamentally reorganize around different evidence.
% FOUNDING_PROBLEM: After several high-profile accidents in the 1970s-80s (Three Mile Island, early aviation CRM era), regulators and industries recognized that licensing exams and seniority were poor predictors of current competence. They needed a way to verify that certified personnel could still perform under pressure without waiting for accidents to reveal decay.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (inadequate competence verification) is attested by accident investigation boards (Kemeny Commission, AAIB reports) and early CRM pioneers — parties outside the simulation industry. The simulation industry and regulatory bodies attest the problem remains live in its original form. Hybrid-occupation researchers and human factors literature attest the problem has shifted: verification is now possible via multiple mechanisms, but the mandate has not evolved.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the decoupling of simulation spend from demonstrated competence outcomes — fidelity mandates escalate, hours increase, but transfer evidence plateaus. Theater ratio (0.62) is high: a growing share of simulation activity serves compliance documentation rather than skill exercise (repeated scenarios, checkbox debriefs, fidelity theater). Suppression (0.55) is moderate: the constraint persists through regulatory mandate and license coupling, not overt coercion — exit means losing certification. Accessibility collapse (0.45) is moderate: alternatives (line audits, peer review, controlled exposure) exist but lack the single-metric auditability that makes simulation governable. Resistance (0.48) is moderate: operators push back on fatigue and relevance; researchers publish transfer-gap findings; but the compliance regime absorbs critique by raising fidelity requirements.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/auditor seat, the constraint is genuine coordination — it solved the observability problem and provides auditable assurance. From the operator/organization seat, it operates as extraction with a coordination veneer — they pay escalating costs for a proxy metric that may not track the latent variable (competence). The engine computes this divergence; the claimed_type (rope) reflects the regulator's framing while metrics describe the extracted structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation industry and compliance auditors are structural beneficiaries (d near 0.0) — they collect revenue and mandate authority from the constraint. Regulators sit near beneficiary (d ~0.2) — they gain legible oversight. Operating organizations are payers with constrained exit (d ~0.7) — they must comply to operate. Frontline operators are identity-locked payers (d ~0.85) — their license and professional self-concept bind them. Taxpayers are trapped payers (d ~0.9) — no exit, diffuse cost. Real-incident advocates and hybrid proponents are excluded (no seat in the compliance logic). The engine computes per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence verification without accidents) was real and the simulation mandate solved it initially. But the mandate has not evolved as evidence accumulated on simulation's limits (transfer gaps, decay persistence, fidelity diminishing returns). The constraint now persists because the compliance infrastructure (regulators, auditors, vendors) extracts benefit from its stability — mandatrophy resolved as extraction capture. The reading's axioms treat simulation fidelity as the competence variable; this axiom is holdable within the reading but overridden by empirical evidence from outside the beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_gap,
    'What is the actual transfer effectiveness of high-fidelity simulation to real-world performance under novel, high-stress conditions, and does it plateau above a fidelity threshold?',
    'Longitudinal studies correlating simulation metrics with operational performance during real events; controlled experiments with fidelity gradients measuring transfer to untrained scenarios.',
    'If transfer plateaus or decays rapidly, the escalating fidelity mandates are extractive theater; if transfer scales with fidelity, the extraction may be the price of genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_gap, empirical, 'Whether simulation fidelity investment yields proportional competence returns or diminishing/zero returns.').

omega_variable(
    competence_kernel_identifiability,
    'Is the ''competence kernel'' a stable, identifiable latent variable that can be occupied by exercise, or is it a reification of the compliance metric itself?',
    'Cognitive task analysis and longitudinal skill decomposition: if the kernel''s components decay at different rates and respond to different exercise types, the unitary kernel is a regulatory fiction.',
    'If the kernel is a fiction, the constraint coordinates around a non-existent target — pure extraction. If real but multi-dimensional, simulation sufficiency is false; hybrid occupation is structurally necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_kernel_identifiability, conceptual, 'Whether the core theoretical object of the constraint exists as modeled or is a compliance-driven reification.').

omega_variable(
    reading_foreclosure_structure,
    'Does the simulation_sufficiency reading logically foreclose the real_incident_necessity reading within a single regulatory framework, or do they coexist as competing evidence standards?',
    'Analyze regulatory texts: if simulation completion is defined as necessary AND sufficient for competence maintenance, real-incident evidence is structurally excluded (forecloses). If simulation is necessary but not sufficient, they coexist.',
    'Foreclosure would mean the simulation reading structurally displaces the real-incident reading from legitimacy; coexistence means both remain live but compete for resources and mandate scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Structural relationship between this reading and the real_incident_necessity sibling.').

omega_variable(
    internalized_suppression_operators,
    'Is frontline operator compliance driven primarily by structural mandate (license loss) or by internalized belief that simulation is necessary for their own competence?',
    'Anonymous surveys of operators measuring willingness to complete simulation if not mandated; correlation of voluntary simulation use with certification status.',
    'If substantially internalized, suppression is higher than structural measures indicate — operators police themselves. If primarily structural, resistance potential is higher if mandate legitimacy erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_operators, empirical, 'Structural vs. internalized suppression mechanism for identity-locked frontline operators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1980, competence_occupation__simulation_sufficiency, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(comp_tr_t1990, competence_occupation__simulation_sufficiency, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(comp_tr_t2000, competence_occupation__simulation_sufficiency, theater_ratio, 2000, 0.41).
narrative_ontology:measurement(comp_tr_t2010, competence_occupation__simulation_sufficiency, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(comp_tr_t2020, competence_occupation__simulation_sufficiency, theater_ratio, 2020, 0.58).
narrative_ontology:measurement(comp_tr_t2025, competence_occupation__simulation_sufficiency, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(comp_be_t1980, competence_occupation__simulation_sufficiency, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(comp_be_t1990, competence_occupation__simulation_sufficiency, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(comp_be_t2000, competence_occupation__simulation_sufficiency, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(comp_be_t2010, competence_occupation__simulation_sufficiency, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(comp_be_t2020, competence_occupation__simulation_sufficiency, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(comp_be_t2025, competence_occupation__simulation_sufficiency, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1980, competence_occupation__simulation_sufficiency, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(comp_su_t1990, competence_occupation__simulation_sufficiency, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(comp_su_t2000, competence_occupation__simulation_sufficiency, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(comp_su_t2010, competence_occupation__simulation_sufficiency, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(comp_su_t2020, competence_occupation__simulation_sufficiency, suppression_requirement, 2020, 0.53).
narrative_ontology:measurement(comp_su_t2025, competence_occupation__simulation_sufficiency, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.1).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, simulation_fidelity_escalation_mandate).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, training_compliance_audit_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the competence_occupation kernel. The simulation_sufficiency reading claims simulation drills are sufficient (rope-claimed, tangled_rope-measured). The real_incident_necessity reading claims only real events suffice (mountain-claimed by proponents, snare-measured by critics). The hybrid_occupation reading claims multi-mechanism necessity (scaffold-claimed, rope/tangled_rope-measured depending on configuration). They form a constraint family linked by network.affects_constraints. The ε values differ: this reading ε=0.68 (extraction via compliance mandate); real_incident_necessity ε≈0.1 (no mandate, but high cost when events occur); hybrid_occupation ε≈0.35 (distributed coordination cost).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, moderate, 0.85).
constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
