% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness Persistence (Hybrid Reading)
 *   domain: institutional/safety/governance
 *
 * SUMMARY:
 *   Disaster preparedness persists as a stratified regime: engineering
 *   inspections retain operational competence (genuine coordination of
 *   structural safety), while evacuation drills have ritualized — form
 *   persists, but evacuation competence has atrophied. The hybrid reading
 *   holds that the constraint is not uniformly Mountain (competence_reading)
 *   nor uniformly Piton (husk_reading), but a mixed structure where
 *   extraction is localized to the drill subsystem. The constraint's
 *   persistence depends on active enforcement of both components, but the
 *   drill component extracts compliance effort without delivering
 *   proportional readiness — a coordination cover for extraction. The
 *   claimed_type is tangled_rope because the overall regime combines genuine
 *   coordination (inspections) with asymmetric extraction (ritualized drills)
 *   under a single enforcement umbrella.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.45).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness Persistence (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/safety/governance").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '481bbea0-0949-4895-8889-537168b42398').
narrative_ontology:cs_kernel_codification('481bbea0-0949-4895-8889-537168b42398', formalized).
narrative_ontology:cs_authority_grounding('481bbea0-0949-4895-8889-537168b42398', extraction).
narrative_ontology:cs_interpretation_layer_present('481bbea0-0949-4895-8889-537168b42398').
narrative_ontology:cs_reading_relation('481bbea0-0949-4895-8889-537168b42398', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('481bbea0-0949-4895-8889-537168b42398', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('481bbea0-0949-4895-8889-537168b42398', foundational, preparedness_is_stratified_not_uniform).
narrative_ontology:cs_axiom_status(preparedness_is_stratified_not_uniform, holdable).
narrative_ontology:cs_axiom_grounding('481bbea0-0949-4895-8889-537168b42398', preparedness_is_stratified_not_uniform, empirically_contingent).
narrative_ontology:cs_axiom('481bbea0-0949-4895-8889-537168b42398', foundational, engineering_inspection_retains_operational_validity).
narrative_ontology:cs_axiom_status(engineering_inspection_retains_operational_validity, holdable).
narrative_ontology:cs_axiom_grounding('481bbea0-0949-4895-8889-537168b42398', engineering_inspection_retains_operational_validity, empirically_contingent).
narrative_ontology:cs_axiom('481bbea0-0949-4895-8889-537168b42398', foundational, evacuation_drills_exhibit_ritualization_without_proportional_readiness).
narrative_ontology:cs_axiom_status(evacuation_drills_exhibit_ritualization_without_proportional_readiness, holdable).
narrative_ontology:cs_axiom_grounding('481bbea0-0949-4895-8889-537168b42398', evacuation_drills_exhibit_ritualization_without_proportional_readiness, empirically_contingent).
narrative_ontology:cs_reference_frame('481bbea0-0949-4895-8889-537168b42398', post_1990s_regulatory_consolidation).
narrative_ontology:cs_drift_state('481bbea0-0949-4895-8889-537168b42398', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('481bbea0-0949-4895-8889-537168b42398', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, safety_regulators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, engineering_inspection_firms).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, facility_operators).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, drill_participants).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, taxpayers).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, institutional_preparedness_doctrine).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, regulatory_compliance_as_readiness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce building codes, inspection mandates, and drill requirements. Their authority and budget depend on the persistence of the preparedness regime. They control the regulatory agenda and define what counts as compliance.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Administer drill programs, certify compliance, and receive funding tied to preparedness metrics. They benefit from the regime's persistence but are constrained by political oversight and inter-agency coordination demands.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, emergency_management_agencies, beneficiary).

% Contracted to perform structural and systems inspections. They collect fees for a service that retains genuine technical validity. They can exit to adjacent markets (insurance inspection, forensic engineering) if mandates change.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspection_firms, beneficiary,
    organized, biographical, mobile, regional).

% Bear the cost of mandated inspections and drill programs. They value inspections for liability reduction and asset protection but experience drills as disruptive compliance theater. Their exit is constrained by licensing, insurance, and regulatory requirements.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, facility_operators, payer,
    powerful, biographical, constrained, national).

% Employees, students, residents required to participate in evacuation drills. They invest time and attention with no measurable improvement in personal evacuation capability. Opt-out is structurally unavailable — non-participation risks disciplinary or legal consequences.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, drill_participants, payer,
    powerless, immediate, trapped, local).

% Fund emergency management agencies and subsidized inspection programs. They receive diffuse safety benefits but no direct accountability for how preparedness spending translates to outcomes. Exit is constrained to political channels.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, taxpayers, payer,
    organized, generational, constrained, national).

% Academic researchers, NGOs, and professional bodies that evaluate preparedness effectiveness. They have no operational role in the constraint but produce evidence on which components retain validity and which have ritualized.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, independent_safety_auditors, observer,
    analytical, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Engineering inspections coordinate genuine structural safety verification across facilities, ensuring buildings and systems meet load, fire, and environmental standards. Evacuation drills nominally coordinate population movement knowledge and emergency response rehearsal, but in practice this function has ritualized — the coordination signal (drill execution) persists while the operational payload (evacuation competence) has atrophied.
% TRANSFER_FUNCTION: Moves compliance effort and resources from facility operators and drill participants to regulatory agencies and inspection firms. Inspection fees transfer to engineering firms for a service with real technical value. Drill time and administrative overhead transfer to emergency management agencies without proportional readiness return. Taxpayer funds transfer to agency budgets sustained by the preparedness mandate.
% ABSENT_VOICES: Frontline responders (fire, EMS) who observe drill inadequacies but are not consulted on drill design. Communities in high-risk zones that would prefer resource allocation to structural mitigation over repeated evacuation rehearsals. Small facility operators who cannot absorb compliance costs and are driven to informal or non-compliant operation.
% DISAPPEARANCE_RATIONALE: If mandated drills and inspections vanished overnight, engineering inspections would persist in insurance-driven and liability-driven forms (market coordination). Evacuation readiness would degrade rapidly for populations without drills. The regulatory framework would reorganize around post-event liability and insurance pricing rather than pre-event mandate. Emergency management agencies would lose their primary statutory mandate and budget anchor.
% FOUNDING_PROBLEM: Post-disaster investigations (e.g., 1980s-1990s industrial fires, seismic events) revealed two gaps: (1) inadequate structural safety verification for critical facilities, and (2) population evacuation capability that failed under real conditions. The combined mandate was built to solve both through regulated inspection and rehearsed drill.
% FOUNDING_PROBLEM_CORROBORATION: Engineering professional bodies (ASCE, NFPA technical committees) attest that structural inspection remains vital and the founding problem for that component is live. Emergency management researchers (e.g., PERI, Natural Hazards Center) document that drill ritualization has decoupled rehearsal from evacuation competence — the founding problem for drills is dead but the arrangement persists. Disaster survivors' groups attest both gaps: inspections missed critical flaws in some events; drills failed to prepare occupants in others.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).
:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.45) reflects localized extraction: drills extract high effort for low readiness return, while inspections extract fees for genuine service. Suppression (0.55) is moderate — mandates are enforced but alternatives exist (insurance-driven inspection, voluntary drills). Theater_ratio (0.6) is elevated because the drill subsystem is largely performative; inspection theater is lower. Accessibility_collapse (0.5) — facility operators cannot exit mandates, but can choose inspection providers; drill participants are trapped. Resistance (0.4) — facility operators push back on drill frequency/cost; regulators resist drill reform. Measurements show theater and extraction rising over 30 years as drill ritualization intensified while inspection validity held.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/agenda_setter seat, the regime is genuine coordination (inspections work, drills rehearse). From the drill_participant seat, the same regime is enforced extraction (time taken, no capability gained). The facility_operator seat experiences a split: inspections are valued coordination; drills are extraction. The engine computes this divergence from the structural data — the hybrid reading's claim (tangled_rope) captures the structural asymmetry that single-type readings miss.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators and emergency management agencies are structural beneficiaries (agenda_setters who control the mandate and capture budget/compliance flows) — d near 0.1-0.2. Engineering inspection firms are beneficiaries with mobile exit — d near 0.15. Facility operators are payers with constrained exit (powerful but license-bound) — d near 0.7. Drill participants are payers with trapped exit (powerless, no opt-out) — d near 0.95. Taxpayers are diffuse payers with constrained political exit — d near 0.6. Independent auditors are analytical observers — d = 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The drill subsystem exhibits mandatrophy: its founding problem (evacuation competence) is dead or contested, but the mandate persists and extracts compliance. The inspection subsystem does not — its founding problem (structural verification) remains live per engineering bodies. The hybrid reading prevents mislabeling the entire regime as pure extraction (snare) or pure coordination (rope) by localizing extraction to the ritualized subsystem. The tangled_rope classification captures this stratification: coordination and extraction are real and co-present, not mutually exclusive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the hybrid reading''s structural claim (stratified Mountain/Piton) represent a distinct constraint from the competence_reading and husk_reading, or a synthesis that resolves their opposition?',
    'Compare the three readings'' ε values, beneficiary/victim sets, and drift trajectories. If hybrid_reading''s ε is not a convex combination of the siblings'', and its victim set differs structurally (localized to drill participants), it is a distinct constraint per ε-invariance.',
    'If distinct, the three readings form a constraint family linked by network.affects_constraints. If synthetic, hybrid_reading may be an analytical overlay rather than an independently instantiated constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the hybrid reading is a third independently instantiated constraint or an analytical synthesis of the sibling readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by drill_participants structural (mandatory attendance, disciplinary consequences) or internalized (belief that drills are necessary, identity as ''prepared citizen'')?',
    'Post-mandate suppression trajectory: if drill participation and compliance anxiety persist after mandate removal, reclassify as partially internalized. Survey drill participants on perceived voluntariness and personal evacuation confidence.',
    'If internalized, effective suppression is higher than structural measure suggests — participants carry the constraint internally after formal exit. This would amplify χ for the powerless seat beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the most trapped stakeholder.').

omega_variable(
    extraction_localization_boundary,
    'Where exactly does the extraction boundary lie within the preparedness regime? Does it extend beyond drills to inspection components (e.g., mandated inspection frequency exceeding technical need)?',
    'Compare mandated inspection intervals to failure-rate data and insurance-driven inspection schedules. If mandates exceed risk-optimal frequency, extraction extends into the inspection subsystem.',
    'If extraction extends to inspections, the hybrid reading''s claim (extraction localized to drills) is falsified; the constraint trends toward husk_reading (uniform Piton). If inspections remain at technical optimum, the stratification claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_localization_boundary, empirical, 'Whether extraction is truly confined to the drill subsystem or bleeds into inspection mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_tr_t6, preparedness_persistence__hybrid_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_tr_t12, preparedness_persistence__hybrid_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_tr_t18, preparedness_persistence__hybrid_reading, theater_ratio, 18, 0.51).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_tr_t24, preparedness_persistence__hybrid_reading, theater_ratio, 24, 0.56).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_tr_t30, preparedness_persistence__hybrid_reading, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_be_t6, preparedness_persistence__hybrid_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_be_t12, preparedness_persistence__hybrid_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_be_t18, preparedness_persistence__hybrid_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_be_t24, preparedness_persistence__hybrid_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_be_t30, preparedness_persistence__hybrid_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_su_t6, preparedness_persistence__hybrid_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_su_t12, preparedness_persistence__hybrid_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_su_t18, preparedness_persistence__hybrid_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_su_t24, preparedness_persistence__hybrid_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(preparedness_persistence__hybrid_reading_su_t30, preparedness_persistence__hybrid_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, building_code_enforcement).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, emergency_funding_allocation).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, insurance_underwriting_standards).

% DUAL FORMULATION NOTE:
% This hybrid_reading decomposes the preparedness_persistence kernel into stratified components. competence_reading claims uniform Mountain (low ε, universal coordination). husk_reading claims uniform Piton (moderate ε, diffuse extraction, high theater). hybrid_reading claims Tangled Rope (coordination in inspections, extraction in drills). The three readings form a constraint family: competence_reading → hybrid_reading → husk_reading in extraction escalation. Each has distinct ε and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, powerful, 0.65).
constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
