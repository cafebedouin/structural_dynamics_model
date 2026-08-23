% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Competence Refresh Mandate
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-hazard industries operate under mandates requiring recurring drill
 *   cycles to maintain operator competence for rare catastrophic scenarios.
 *   The continuous_refresh_hybrid reading holds that simulation is necessary
 *   (provides safe practice environment) but not sufficient (fidelity gaps,
 *   scenario coverage limits, and decay dynamics require continuous cycles,
 *   not one-time validation). This reading sits between two siblings:
 *   simulation_as_proxy (simulation counts as valid exercise) and
 *   real_catastrophe_only (only real events exercise competence). The
 *   constraint extracts significant resources from operating organizations
 *   and frontline operators while coordinating a genuine safety function. The
 *   mandate persists because the coordination function is real and the
 *   extraction is tolerated — a classic tangled rope. Theater ratio has risen
 *   as compliance-driven 'check-the-box' drills replace competence-driven
 *   practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.62).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.58).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Competence Refresh Mandate").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, '1a912c65-66e2-4bdf-93dd-ee2ae6668df0').
narrative_ontology:cs_kernel_codification('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', formalized).
narrative_ontology:cs_authority_grounding('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', extraction).
narrative_ontology:cs_interpretation_layer_present('1a912c65-66e2-4bdf-93dd-ee2ae6668df0').
narrative_ontology:cs_reading_relation('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', competence_exercise_validity__simulation_as_proxy, coexists_with).
narrative_ontology:cs_reading_relation('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', foundational, competence_is_process_dependent).
narrative_ontology:cs_axiom_status(competence_is_process_dependent, holdable).
narrative_ontology:cs_axiom_grounding('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', competence_is_process_dependent, empirically_contingent).
narrative_ontology:cs_axiom('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', foundational, simulation_necessary_insufficient).
narrative_ontology:cs_axiom_status(simulation_necessary_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', simulation_necessary_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', continuous_refresh_mandate).
narrative_ontology:cs_drift_state('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', contemporary_compliance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1a912c65-66e2-4bdf-93dd-ee2ae6668df0', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, public).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_professionals).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, operating_organizations).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, simulation_vendors).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, competence_decay_without_practice).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, simulation_fidelity_thresholds_exist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate drill frequency, scope, and fidelity standards through regulations and inspection regimes. Their authority derives from accident investigation findings. They bear political cost if drills are seen as excessive but face blame if competence gaps cause accidents. They set the agenda but do not directly pay for drills.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Bear the direct costs of drill programs: downtime, instructor fees, simulation facility contracts, documentation, and compliance overhead. They can defer or minimize drills within regulatory tolerance but face enforcement action if caught. Exit means exiting the regulated industry entirely. They argue drill requirements exceed marginal safety benefit at current frequencies.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operating_organizations, payer,
    powerful, biographical, constrained, national).

% Lose operational shifts to drill participation; experience fatigue from repetitive scenarios that may not match their actual risk profile. Their competence is the object of the constraint. They cannot easily exit without leaving their profession. Some value the practice; others experience it as ritualized compliance. Their voice in drill design is minimal.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Design, deliver, and audit drill programs. Their professional standing and revenue depend on the mandate's continuity and expansion. They advocate for higher fidelity, more frequent cycles, and broader scenario coverage. They can move between industries and consultancies. Their interest aligns with the coordination function but their income scales with extraction.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_professionals, beneficiary,
    organized, biographical, mobile, national).

% Receive the diffuse safety benefit of maintained competence in high-hazard industries (nuclear, chemical, aviation, rail). They bear the consequence of competence failure as casualties or environmental damage. They have no voice in drill design, no exit from the risk, and no visibility into whether drills are effective or performative.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, public, beneficiary,
    powerless, generational, trapped, national).

% Provide simulator hardware, software, scenario libraries, and instructor services. Their market exists because of the mandate. They lobby for higher fidelity requirements and expanded scope. They can pivot to adjacent markets (training, gaming, defense). Their interest is in the constraint's expansion, not its optimization.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Lack capital for high-fidelity simulators or dedicated drill programs. They comply through tabletop exercises or shared regional facilities that may not reflect their specific hazards. They would argue for proportional requirements but are not consulted in rulemaking. Their exclusion is structural: the mandate assumes organizational scale.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, small_operators, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational competence for rare high-stakes events through structured, recurring practice cycles that prevent skill atrophy and latent failure accumulation between actual incidents.
% TRANSFER_FUNCTION: Moves time, operational capacity, and budget from production to drill cycles; moves risk reduction from organizations to the public; moves revenue to simulation vendors and safety professionals; moves regulatory legitimacy to the mandate itself.
% ABSENT_VOICES: Small operators who cannot afford mandated fidelity; frontline operators who experience drill fatigue and scenario mismatch; communities near hazard sites who bear residual risk but have no seat at standard-setting tables.
% DISAPPEARANCE_RATIONALE: If the continuous refresh mandate vanished overnight, organizations would immediately defer or eliminate drill cycles to recover production time and cost. Competence would decay on a 2-5 year horizon. Latent failures would accumulate undetected. The next rare event would find operators unprepared. Accident rates would rise. The mandate is load-bearing for the current safety record.
% FOUNDING_PROBLEM: Post-accident investigations (Three Mile Island 1979, Bhopal 1984, Challenger 1986, Texas City 2005) repeatedly found that operators had lost competence for scenarios they had trained for once but not rehearsed. The founding problem: one-time validation does not sustain competence for rare events; decay is inevitable without structured refresh.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards (NTSB, CSB, RAIB, JIAAC) consistently cite competence decay in findings. Independent human factors research (e.g., Reason, Hollnagel, Dekker) demonstrates skill atrophy curves. The operators and regulators who benefit from the mandate also cite it, but the corroboration comes from bodies whose mandate is investigation, not regulation.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial resource transfer from operations to drills, vendor markets, and compliance overhead. Suppression (0.58) is moderate: organizations can choose drill design within standards, but the mandate itself is non-negotiable and enforced. Theater ratio (0.42) has climbed steadily as regulatory metrics shifted from competence demonstration to hours/log completion. Accessibility collapse (0.55) is partial: on-the-job experience and near-miss learning provide alternative competence paths but are discounted by regulators. Resistance (0.48) is significant but channeled into compliance minimization rather than open challenge. The claimed type tangled_rope captures the dual nature: real coordination function (competence maintenance) with asymmetric extraction (organizations/operators pay, vendors/professionals/regulators benefit).
 *
 * PERSPECTIVAL GAP:
 *   From the regulator seat, the constraint is a rope: it coordinates a genuine safety need with minimal coercion (standards are consensus-based, fidelity is evidence-based). From the operating organization seat, it is a snare: the mandate extracts increasing resources while the marginal safety benefit of each additional drill cycle is unproven. From the frontline operator seat, it oscillates: valuable practice when scenarios match their reality; theater when they don't. The engine will compute these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators are agenda_setters with analytical exit — they author the constraint and face no personal cost. Operating organizations are powerful payers with constrained exit — they bear cost but can optimize within rules. Frontline operators are moderate payers with constrained exit — they bear time/fatigue cost and cannot leave without career change. Safety professionals and simulation vendors are organized beneficiaries with mobile exit — they capture revenue and can pivot. The public is a powerless beneficiary with trapped exit — they receive safety benefit but cannot opt out of risk. Small operators are excluded powerless trapped actors — they bear disproportionate compliance burden. Directionality flows from organizations/operators (high d) toward regulators/vendors/professionals (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence decay between rare events) remains live — rare events are still rare, and human skill still atrophies. However, the mandate has expanded beyond the founding problem's scope: drill frequency and fidelity requirements have grown faster than evidence of marginal benefit. The mandate now serves vendor markets and professional guilds as much as competence maintenance. This is mandatrophy: the arrangement persists because the coordination function is real, but the extraction layer has thickened beyond what the founding problem justifies. The theater ratio trajectory confirms this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'At what fidelity threshold does simulation become sufficient for competence maintenance, eliminating the need for continuous cycles?',
    'Controlled studies comparing competence retention curves across fidelity tiers (tabletop, part-task, full-mission, virtual reality) over multi-year horizons with transfer-to-real-event measures.',
    'If a sufficient fidelity threshold exists, the continuous mandate could be replaced by periodic high-fidelity validation, reducing extraction. If no threshold exists, continuous cycles are structurally necessary and the mandate''s coordination function is irreducible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether simulation fidelity can substitute for cycle frequency.').

omega_variable(
    marginal_benefit_curve,
    'Does the marginal safety benefit of each additional drill cycle eventually fall below its marginal cost, and has the mandate already passed that point?',
    'Cost-benefit analysis using probabilistic risk assessment with competence decay models, calibrated against incident databases and near-miss reporting systems.',
    'If marginal benefit < marginal cost, the mandate has become extractive beyond its coordination function — a tangling toward snare. If benefit > cost, the current extraction is justified coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_benefit_curve, empirical, 'Whether the mandate''s extraction has exceeded its coordination value.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of competence_exercise_validity disagree structurally?',
    'Map each reading''s implied constraint structure: beneficiary/victim sets, enforcement requirements, coordination claims, and extraction profiles. Identify which structural elements differ.',
    'If readings differ only in emphasis (same structure, different weight), they are perspectives on one constraint. If they differ in beneficiary/victim structure or enforcement logic, they are distinct constraints requiring separate stories — which this JSON already assumes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Structural locus of disagreement among kernel readings.').

omega_variable(
    internalized_suppression_in_operators,
    'Do frontline operators internalize the drill mandate as professional identity (''this is what competent operators do''), making suppression partly self-enforcing?',
    'Longitudinal interview studies tracking operator attitudes toward drills across career stages, correlated with voluntary participation in non-mandated practice.',
    'If suppression is partly internalized, the constraint''s effective suppression is higher than structural measures suggest — operators police themselves. This would increase effective extraction for the payer seat without increasing formal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_in_operators, empirical, 'Structural vs. internalized suppression mechanism for frontline operators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.18).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_tr_t8, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 8, 0.24).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_tr_t16, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 16, 0.31).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_tr_t24, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 24, 0.37).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_tr_t32, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 32, 0.4).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_tr_t40, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_be_t8, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_be_t16, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_be_t24, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_be_t32, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_be_t40, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_su_t8, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_su_t16, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_su_t24, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_su_t32, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(competence_exercise_validity__continuous_refresh_hybrid_su_t40, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__continuous_refresh_hybrid, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, regulatory_capture_via_safety_standards).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, simulation_vendor_market_consolidation).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, operator_fatigue_management_rules).

% DUAL FORMULATION NOTE:
% This constraint is one member of the competence_exercise_validity kernel family. The simulation_as_proxy reading (lower extractiveness, lower suppression, claimed_type: rope) and real_catastrophe_only reading (higher suppression, claimed_type: snare) are linked via affects_constraints. All three share the referent (competence validation for rare events) but author different ε and structural profiles per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__continuous_refresh_hybrid, organized, 0.25).
constraint_indexing:directionality_override(competence_exercise_validity__continuous_refresh_hybrid, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
