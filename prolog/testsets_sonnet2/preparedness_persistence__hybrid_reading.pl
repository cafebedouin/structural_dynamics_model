% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Disaster Preparedness Regime — Stratified Competence/Ritual Hybrid Reading
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   A metropolitan disaster-preparedness regime bundles structural
 *   engineering inspection and evacuation drilling under a single
 *   'preparedness compliance' certificate used by regulators and insurers.
 *   Inspection remains rigorously exercised: it faces continuous
 *   falsification pressure from physical failure, is administered by
 *   credentialed engineers, and triggers costly remediation when it finds
 *   problems. Evacuation drilling, administered by a separate emergency
 *   management agency under the same umbrella, has calcified into scripted,
 *   unvaried, schedule-driven exercises that satisfy audit requirements
 *   without measurably improving real evacuation outcomes. The composite
 *   certificate treats both as equivalent evidence of 'preparedness,' which
 *   is the extraction point: taxpayers and occupants fund and rely on a
 *   signal that is only half load-bearing.
 *
 * KEY AGENTS:
 *   - structural_engineering_inspectorates: primary competent-subsystem administrator, institutional/analytical exit
 *   - emergency_management_agencies: primary ritualized-subsystem administrator, institutional/constrained exit
 *   - insurance_underwriters: beneficiary of the bundled certificate, organized/arbitrage exit
 *   - building_occupants_relying_on_drills: primary bearer of drill-subsystem risk, powerless/trapped
 *   - frontline_evacuation_coordinators: knows the ritual is hollow but lacks authority to fix it
 *   - municipal_taxpayers_funding_theater: funds both subsystems without disaggregated visibility
 *   - policy_analysts_and_auditors: analytical observer, sees the split but rarely reports it disaggregated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.38).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Disaster Preparedness Regime — Stratified Competence/Ritual Hybrid Reading").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1').
narrative_ontology:cs_kernel_codification('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', formalized).
narrative_ontology:cs_authority_grounding('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', expertise).
narrative_ontology:cs_interpretation_layer_present('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1').
narrative_ontology:cs_reading_relation('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', preparedness_persistence__husk_reading, influences).
narrative_ontology:cs_reading_relation('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', preparedness_persistence__competence_reading, influences).
narrative_ontology:cs_axiom('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', foundational, preparedness_regimes_are_structurally_decomposable).
narrative_ontology:cs_axiom_status(preparedness_regimes_are_structurally_decomposable, holdable).
narrative_ontology:cs_axiom_grounding('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', preparedness_regimes_are_structurally_decomposable, empirically_contingent).
narrative_ontology:cs_axiom('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', secondary, composite_certification_obscures_subsystem_divergence).
narrative_ontology:cs_axiom_status(composite_certification_obscures_subsystem_divergence, holdable).
narrative_ontology:cs_axiom_grounding('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', composite_certification_obscures_subsystem_divergence, empirically_contingent).
narrative_ontology:cs_reference_frame('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', dual_mandate_founding_charter).
narrative_ontology:cs_drift_state('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', contemporary_audit_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7d65e0d1-d90a-4d5b-8cc1-12b5670a97b1', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, structural_engineering_inspectorates).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, insurance_underwriters).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, building_occupants_relying_on_drills).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_evacuation_coordinators).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, municipal_taxpayers_funding_theater).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, preparedness_regimes_are_not_monolithic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct load-bearing, seismic, and material-fatigue inspections against codified engineering standards, with findings that trigger real remediation orders and liability exposure. Their competence is exercised continuously against physical failure modes that punish error visibly and quickly, which keeps the discipline live. They administer this subsystem and are credentialed to keep administering it.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, structural_engineering_inspectorates, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, structural_engineering_inspectorates, beneficiary).

% Design and mandate evacuation drills, run tabletop exercises, and certify compliance for buildings and municipalities. Drill scheduling, attendance logging, and after-action reports have become the deliverable in place of measured improvement in actual evacuation speed or route knowledge. They administer this subsystem and could redesign it, but redesign is politically costly and the current form satisfies audit requirements.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, emergency_management_agencies, beneficiary).

% Price risk using inspection certificates and drill-compliance checkboxes as underwriting inputs, largely indifferent to whether the drill component reflects genuine readiness. Benefit from the appearance of a unified preparedness regime without bearing the cost of verifying which half is real.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, insurance_underwriters, beneficiary,
    organized, biographical, arbitrage, national).

% Participate in scheduled evacuation drills that have calcified into rote, low-attention rituals — same route, same time slot, no scenario variation, no measured egress-time improvement. In an actual event they would be relying on muscle memory built against conditions that may not match. They cannot opt out without risking employment or tenancy consequences, and cannot independently verify whether the drill regime would actually help them.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, building_occupants_relying_on_drills, payer,
    powerless, immediate, trapped, local).

% Run the drills as scripted and privately know the exercises no longer test anything — routes are memorized, timing is theatrical, and feedback that would improve the drill design is not incorporated because it would require budget and schedule changes the agency resists. They absorb the reputational risk if a real evacuation goes badly, without the authority to redesign the exercise.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_evacuation_coordinators, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, frontline_evacuation_coordinators, excluded).

% Fund both subsystems through the same preparedness budget line without visibility into which dollars produce inspected structural safety and which fund ritualized drill logistics. Cannot direct funding away from the atrophied component because the two are bundled in a single line item and a single agency mandate.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, municipal_taxpayers_funding_theater, payer,
    powerless, generational, trapped, regional).

% Review after-action reports and inspection records across jurisdictions, positioned to see that the two subsystems diverge in operational value but rarely disaggregate them in public reporting, since 'preparedness compliance' is reported as a single composite score.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, policy_analysts_and_auditors, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates two different problems under one institutional umbrella: preventing structural failure (solved by continuous, competence-testing inspection) and enabling rapid human evacuation (nominally solved by drilling, but the drilling component has stopped functioning as a competence-testing mechanism).
% TRANSFER_FUNCTION: Moves inspection fees and compliance budgets from taxpayers and building owners to inspectorates (who deliver real safety verification) and to emergency management agencies (who deliver largely ritualized drill administration), with insurers extracting a pricing benefit from the bundled certification without disaggregating the two.
% ABSENT_VOICES: Building occupants and evacuation coordinators would say the drill component needs scenario variation and outcome measurement, but they are not consulted on drill design, which is set top-down by the emergency management agency for audit-compliance reasons rather than readiness reasons.
% DISAPPEARANCE_RATIONALE: If the structural-inspection subsystem vanished, the world would visibly rearrange — buildings would fail, insurers would reprice risk sharply, remediation would become urgent. If the drill subsystem vanished, the immediate operational world would barely change (occupants already do not rely on it for real readiness), but the compliance/insurance/audit apparatus built on top of it would need to find a new proxy for 'preparedness,' so the administrative world would rearrange even though the safety world would not.
% FOUNDING_PROBLEM: Both subsystems were founded to prevent mass-casualty events: structural inspection to prevent building failure, evacuation drilling to ensure occupants could exit quickly and correctly under panic conditions.
% FOUNDING_PROBLEM_CORROBORATION: Structural engineers and licensing boards (external professional bodies, not the inspecting agency itself) corroborate that the inspection subsystem's founding problem remains live and is being actively solved. For the drill subsystem, independent post-incident reviews and academic evacuation-behavior researchers (outside both the emergency management agency and the insurers who benefit from the compliance certificate) report that scripted, unvaried drills produce negligible improvement in real evacuation outcomes — corroborating that the drill subsystem's founding problem has drifted toward dead-but-administratively-live status, distinct from the still-live inspection subsystem.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) and theater_ratio (0.48) are authored at levels reflecting that roughly half the regime — the drill subsystem — has drifted toward pure ritual, while the inspection subsystem pulls the composite average down from what a pure husk_reading would score. Suppression (0.38) is moderate: occupants cannot easily opt out of drills (institutional/employment pressure) but face no comparable suppression around inspection compliance, which they mostly do not interact with directly. Accessibility_collapse (0.4) and resistance (0.45) sit mid-range because alternatives to the drill ritual (scenario-varied, outcome-measured exercises) are known and occasionally proposed by coordinators, but institutional inertia and audit-driven budgeting suppress adoption without fully foreclosing it. The rising theater_ratio and base_extractiveness trajectories model the drill subsystem's gradual ritualization while the inspection subsystem holds steady — the composite score drifts upward as the ritual component's share of total 'preparedness activity' grows relative to genuine inspection effort.
 *
 * PERSPECTIVAL GAP:
 *   From the inspectorate's seat, the regime is a functioning mountain: inspection findings are load-bearing, consequential, and resistant to capture because physical failure punishes shortcuts quickly. From the drill-subsystem seats — coordinators and occupants — the same regime computes closer to piton: the form persists, the audit checkbox gets ticked, but no one meaningfully tests or improves the underlying competence. The composite certificate erases this divergence for insurers and regulators, who read one score where the structure actually contains two.
 *
 * DIRECTIONALITY LOGIC:
 *   Inspectorates and emergency management agencies are declared beneficiaries because they administer their respective subsystems and derive institutional legitimacy (and, for inspectorates, genuine safety outcomes) from doing so — though the engine should read them very differently given the competence divergence. Insurance underwriters are a secondary beneficiary: they extract pricing confidence from the bundled certificate without bearing verification cost. Building occupants, coordinators, and taxpayers are victims specifically of the drill subsystem's ritualization — they pay (through drill time, career risk, and tax dollars) for a signal that in the drill component no longer reliably predicts the outcome it claims to predict.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading exists precisely to prevent two mislabeling errors: reading the whole regime as extractive because one subsystem (drills) has hollowed out — which would wrongly discredit the inspectorate's genuine, still-functioning competence work — and reading the whole regime as sound because one subsystem (inspection) remains rigorous — which would wrongly launder the drill subsystem's ritualization under the inspectorate's credibility. Disaggregation is the mandatrophy resolution: the founding problem is live for inspection and dead-but-administratively-persisting for drills, and treating 'preparedness' as one undifferentiated mandate obscures that split.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_location,
    'Is the stratification claimed by this hybrid reading a stable structural fact about preparedness regimes generally, or an artifact of this particular jurisdiction''s administrative bundling choice?',
    'Cross-jurisdictional comparison: examine whether other preparedness regimes that separate inspection and drilling into independently budgeted and independently audited subsystems show the same competence/ritual divergence, or whether divergence only appears under bundled administration.',
    'If divergence appears even under unbundled administration, the hybrid reading generalizes as a structural claim about the two activity types themselves (inspection is inherently falsification-tested; drilling is inherently prone to ritualization absent scenario variation). If divergence disappears under unbundled administration, the extraction is an artifact of bundling and the fix is administrative separation rather than a claim about the activities'' intrinsic nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, empirical, 'Whether hybrid stratification is intrinsic to the activity types or an artifact of bundled administration — the structural delta this reading is staked on.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the competence_reading and husk_reading readings locate their disagreement with this hybrid reading — do they dispute the factual claim that inspection and drilling diverge, or do they dispute which subsystem should be treated as representative of ''preparedness'' as a category?',
    'This is committer structure, not resolvable by additional measurement of the constraint itself — it requires examining what each sibling reading treats as the representative case when generalizing to the whole regime.',
    'If the siblings dispute the factual divergence, this is an empirical contest resolvable by outcome data (as in the omega above). If the siblings agree on the divergence but disagree on which subsystem should set the framing for ''is preparedness real,'' the disagreement is conceptual/political about how composite institutional signals should be scored, and no amount of inspection or drill-outcome data resolves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Locates whether the kernel contest is empirical (do the subsystems diverge) or conceptual (which subsystem is representative).').

omega_variable(
    drill_subsystem_recoverability,
    'Can the drill subsystem''s ritualized state be reversed through redesign (scenario variation, outcome measurement, decoupling from audit-checkbox incentives), or has it passed a point where institutional incentives permanently favor the low-cost ritual form?',
    'Pilot programs introducing unannounced, scenario-varied drills with measured egress-time outcomes in a subset of jurisdictions, compared against continued scripted drills, tracked over multiple years for whether the piloted subsystem is sustained or reverts to ritual once initial funding/attention lapses.',
    'If recoverable, the drill subsystem is better classified as a degraded scaffold or recoverable piton — mandatrophy is resolvable through design intervention. If not recoverable under current institutional incentives, the drill subsystem is a stable piton requiring structural (not merely design) reform, such as decoupling audit certification from the same agency that administers the drills.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drill_subsystem_recoverability, empirical, 'Whether the ritualized drill subsystem''s degradation is reversible by redesign or structurally locked in by incentive architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__hybrid_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__hybrid_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__hybrid_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__hybrid_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__hybrid_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__hybrid_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__hybrid_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__hybrid_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t8, preparedness_persistence__hybrid_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(prep_su_t16, preparedness_persistence__hybrid_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__hybrid_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(prep_su_t32, preparedness_persistence__hybrid_reading, suppression_requirement, 32, 0.37).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).

% DUAL FORMULATION NOTE:
% This story is the hybrid_reading member of the preparedness_persistence kernel family (3 readings). competence_reading treats the regime as uniformly live-exercised (mountain-leaning across both subsystems); husk_reading treats it as uniformly ritualized (piton-leaning across both subsystems); this hybrid_reading claims the regime is genuinely stratified — inspection competent, drilling ritualized — and that neither uniform reading is accurate. All three readings share the same underlying regime as referent but author different ε, different beneficiary/victim structures, and different claimed_type. Link all three via affects_constraints; each documents the kernel contest in commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
