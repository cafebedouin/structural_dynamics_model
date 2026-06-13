% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-as-Sufficient Competence Retention Framework
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, healthcare, emergency
 *   response) face the problem of maintaining catastrophe-avoidance
 *   competence during long periods without actual catastrophes. This
 *   constraint instantiates ONE READING of a contested kernel about how
 *   competence should be retained and validated: the assertion that
 *   high-fidelity simulation constitutes genuine exercise of
 *   catastrophe-avoidance competence because the cognitive and procedural
 *   demands are structurally equivalent to real events. This reading shapes
 *   training pathways, resource allocation, certification standards, and the
 *   relative authority of different knowledge sources. Three readings of the
 *   kernel coexist: (1) 'catastrophe_as_necessary' — only real catastrophes
 *   provide the visceral stakes and uncontrolled learning necessary for
 *   genuine competence; (2) 'near_miss_as_bridge' — near-miss incidents and
 *   minor failures provide sufficient real-world feedback and tacit-knowledge
 *   development without catastrophes; (3) 'simulation_as_sufficient' —
 *   simulation is structurally equivalent and constitutes genuine competence.
 *   This story models reading (3) as a tangled rope: it solves a genuine
 *   coordination problem (training people without waiting for catastrophes)
 *   while creating asymmetric extraction (simulator operators and training
 *   standardizers gain permanent institutional authority; field practitioners
 *   lose autonomy; tacit-knowledge development is suppressed). The theater
 *   ratio rises from 0.35 to 0.58 over 40 years, indicating that simulator
 *   activity increasingly becomes certification-passing and audit-compliance
 *   rather than genuine skill-building — a characteristic trajectory for
 *   tangled ropes aging toward piton-hood. The constraint requires active
 *   enforcement because field practitioners and incident-learning networks
 *   continuously push back against the equivalence claim.
 *
 * KEY AGENTS:
 *   - simulation_infrastructure_operators: institutional beneficiary and agenda-setter; directly gains budget, permanent staff, and authority from constraint expansion; can exit to rival organizations
 *   - training_standardizers: institutional beneficiary; gains power to define competence through portable metrics; mobile power level — can shift to another standards body
 *   - field_practitioners: moderate-power payers; constrained exit (certification requirement); bear the cost of simulator-field mismatches and reduced tacit-knowledge development
 *   - operational_safety_culture: powerless, identity-locked payer; suppressed by the assertion that simulator performance is sufficient; represents the cumulative, tacit-knowledge traditions of field learning
 *   - near_miss_learning_networks: excluded; represent the alternative competence pathway; structurally prevented from participating in competence definition
 *   - catastrophe_researchers: observers; can measure whether the reading's core claim is empirically true (whether simulator-trained practitioners have equivalent competence to field-experienced or near-miss-trained cohorts)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.61).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.72).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.61).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-as-Sufficient Competence Retention Framework").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, 'dc5d349d-ee3a-4c08-a5d7-f6215365414c').
narrative_ontology:cs_kernel_codification('dc5d349d-ee3a-4c08-a5d7-f6215365414c', fixed_text).
narrative_ontology:cs_authority_grounding('dc5d349d-ee3a-4c08-a5d7-f6215365414c', extraction).
narrative_ontology:cs_interpretation_layer_present('dc5d349d-ee3a-4c08-a5d7-f6215365414c').
narrative_ontology:cs_reading_relation('dc5d349d-ee3a-4c08-a5d7-f6215365414c', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('dc5d349d-ee3a-4c08-a5d7-f6215365414c', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('dc5d349d-ee3a-4c08-a5d7-f6215365414c', foundational, simulator_cognitive_demand_equivalence).
narrative_ontology:cs_axiom_status(simulator_cognitive_demand_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('dc5d349d-ee3a-4c08-a5d7-f6215365414c', simulator_cognitive_demand_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('dc5d349d-ee3a-4c08-a5d7-f6215365414c', secondary, procedural_standardization_prerequisite).
narrative_ontology:cs_axiom_status(procedural_standardization_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('dc5d349d-ee3a-4c08-a5d7-f6215365414c', procedural_standardization_prerequisite, instrumental).
narrative_ontology:cs_reference_frame('dc5d349d-ee3a-4c08-a5d7-f6215365414c', simulator_sufficiency_premise).
narrative_ontology:cs_drift_state('dc5d349d-ee3a-4c08-a5d7-f6215365414c', contemporary_operational_safety_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dc5d349d-ee3a-4c08-a5d7-f6215365414c', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulation_infrastructure_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_standardizers).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, field_practitioners).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, operational_safety_culture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, maintains, validates, and expands high-fidelity simulation environments. Publishes research supporting simulator-sufficiency claims. Sits on standards bodies and certification boards. Directly controls what scenarios practitioners train on and what metrics count as 'competent.' Benefits from constraint expansion because larger simulator infrastructure = larger budgets, more permanent staff, more institutional authority. Can shift to competing organizations if their current employer invests less in simulation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulation_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Sets curricula, passing scores, and competence standards. Maintains the certification apparatus. Benefits from simulator-sufficiency because simulator metrics are portable, auditable, and easy to defend in regulatory contexts. Field judgment and near-miss learning are ambiguous and hard to standardize. Can move to another standardizing body if their current one deprioritizes simulators.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_standardizers, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, training_standardizers, agenda_setter).

% Operate in real environments where context-specific judgment and tacit knowledge accumulated through experience matter. Must pass simulator benchmarks to maintain certification and employment. Invest time in both simulator training (to pass certification) and field learning (to be actually competent). Bear the cost of gaps between simulator scenarios and real-world complexity. Cannot exit the constraint: leaving professional practice means loss of livelihood; refusing simulator training means loss of certification. Carry responsibility for real consequences; their competence is measured by simulator performance rather than real outcomes.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, field_practitioners, payer,
    moderate, biographical, constrained, local).

% The shared epistemic and procedural traditions that accumulate across field experience, incident analysis, and embodied learning in high-stakes contexts. Includes tacit knowledge about how systems actually fail, pattern recognition developed over decades, and the moral gravity of responsibility. The constraint suppresses this by institutionalizing the claim that simulator performance is equivalent. Practitioners internalize that field judgment is secondary to metrics, reducing investment in incident-based learning networks. Over time, the organizational memory of tacit competence-building erodes.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operational_safety_culture, payer,
    powerless, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(competence_retention_exercise__simulation_as_sufficient, operational_safety_culture).

% The ultimate purpose: keeping practitioners competent to prevent catastrophes without requiring that catastrophes occur to teach the lesson. This is a vindicated proposition — if the constraint achieves catastrophe prevention, it succeeds. The question is whether simulator-sufficiency is the best pathway to this goal, or whether alternative readings (near-miss learning, field experience) achieve it more effectively.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, catastrophe_prevention_goal, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(competence_retention_exercise__simulation_as_sufficient, catastrophe_prevention_goal).

% Root-cause investigation teams, incident learning forums, and cross-organizational lesson-sharing mechanisms. Excluded because the constraint's logic treats simulator performance as the primary competence measure, making field-sourced learning secondary. They would argue that incidents reveal failure modes simulators miss, and that structured near-miss analysis maintains competence just as effectively as simulation. Their participation in competence-definition would require admitting that non-simulator pathways are valid, which would undermine the constraint.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, near_miss_learning_networks, excluded,
    organized, biographical, constrained, local).

% Study why catastrophes occur, what competencies prevent them, and how training shapes actual performance. Can measure whether simulator-trained practitioners have equivalent, better, or worse catastrophe-prevention outcomes compared to field-experienced or near-miss-trained cohorts. Provide external validation or refutation of the constraint's core claim: that simulator performance correlates with or constitutes genuine competence.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, catastrophe_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, simulation_infrastructure_operators).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining catastrophe-avoidance competence during long quiet periods when actual catastrophes do not occur to refresh skills. Enables organizations to train systematically and repeatedly on low-frequency, high-consequence scenarios without waiting for real events. Provides a repeatable, auditable pathway for competence validation.
% TRANSFER_FUNCTION: Moves authority to define and measure competence from field practitioners and incident-learning networks to simulation engineers and training standardizers. Moves resources from near-miss investigation and tacit-knowledge documentation to simulator platform development and maintenance. Moves responsibility for 'readiness' from professional judgment to simulator score thresholds. Moves institutional legitimacy from incident-based organizational learning to audit-certified training completion.
% ABSENT_VOICES: Practitioners trained primarily through near-miss incidents and field experience; organizations that maintain competence successfully through incident learning networks rather than simulation; researchers who have documented failure modes in simulator-trained cohorts; tacit-knowledge holders whose expertise was never standardizable and thus invisible to certification systems. These voices would attest that simulator training is necessary but not sufficient; that context-specific judgment cannot be fully standardized; and that organizations relying exclusively on simulators show competence gaps in novel scenarios where field experience would help.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if simulator performance were no longer sufficient grounds for certification — training pathways would diversify. Organizations would invest more heavily in near-miss learning networks, incident investigation capability, and field-experience requirements for advancement. Simulator investment would decline or shift from primary competence vehicle to supplementary tool. Competence definition would reintegrate field judgment, tacit knowledge, and incident-sourced learning. The institutional architecture of high-reliability organizations would reorganize around multiple competence pathways rather than a single simulator-validated pathway.
% FOUNDING_PROBLEM: High-reliability organizations must maintain catastrophe-avoidance competence during periods when actual catastrophes do not occur. Practitioners cannot wait years or decades for a real disaster to refresh their competence. How can organizations keep people ready for low-frequency, high-consequence events without either waiting for the events or accepting a different competence-maintenance mechanism?
% FOUNDING_PROBLEM_CORROBORATION: The constraint's defenders and catastrophe researchers both corroborate: competence decay during quiet periods is real and has been documented in actual incidents where long-quiet organizations failed when something did happen. However, near-miss researchers and field practitioners corroborate that the founding problem has alternative solutions that the constraint does not acknowledge: organizations with strong incident investigation cultures maintain competence through near-miss analysis and field-sourced learning during quiet periods. The founding problem is live; the claim that simulation is the uniquely valid solution is contested by external observers and alternative-pathway organizations.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 at t=0 to 0.61 at t=40, with the steepest growth in the first 15 years as simulator infrastructure expands and becomes standardized. Growth plateaus after t=25, indicating the constraint reaches institutional equilibrium once simulator standardization is entrenched and practitioners have internalized that simulator performance is the primary competence measure. Theater ratio shows parallel growth (0.35→0.58), indicating that a growing share of simulator activity is performative validation (passing audits, maintaining certifications) rather than genuine skill-building. This is the Goodhart drift characteristic of tangled ropes: the measurement (simulator score) becomes decoupled from the underlying phenomenon it was supposed to measure (real catastrophe-avoidance ability). Suppression follows the same trajectory (0.55→0.72) because active institutional enforcement is required to prevent field practitioners and incident-learning networks from reclaiming authority over competence definition. Accessibility collapse (0.68) is moderate because near-miss learning remains available as an alternative, but is institutionally suppressed rather than structurally impossible. Resistance (0.54) is moderate-high because field practitioners continuously attest that simulator training is incomplete, and catastrophe researchers measure divergent outcomes between simulator-trained and field-experienced cohorts. The rise in all three enforcement-related metrics (suppression, theater, accessibility collapse) reflects a tangled rope at the inflection point where the real coordination function (training without catastrophes) is being consumed by the extractive function (institutional control over competence definition). One shared time grid is used for all metrics; every metric is authored at every examined time point to enable proper temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   Simulator operators and training standardizers experience this constraint as genuine coordination: they solve a real problem (maintaining competence without catastrophes), maintain control over resources and institutions that depend on simulation, and see certification through simulator performance as enabling safety. Field practitioners experience the same structure as extractive: their field judgment is systematized into subordinate status, their incident-based learning is downgraded relative to simulator scores, and they carry real responsibility while their competence is measured by simulator performance. Catastrophe researchers sit at a seat where they can measure which framing is empirically more accurate: whether practitioners trained primarily through simulators have equivalent or divergent catastrophe-prevention outcomes compared to field-experienced or near-miss-trained practitioners. The engine computes these divergent directionalities from the structural data: simulator operators collect institutional authority and budget (d is low, beneficiary direction); field practitioners lose autonomy and bear the cost of gaps between simulator fidelity and field reality (d is high, target direction). The institutional power asymmetry (institutional vs. moderate power) means simulator operators' preferences shape the constraint's evolution far more than field practitioners' objections.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulator infrastructure operators are structural beneficiaries (d ≈ 0.15, adjusted downward from the analytical default of 0.0 only because they do provide a real service — simulation is necessary, not purely extractive). They set the agenda, maintain institutional structures, gain budget and permanent positions, and have arbitrage-grade exit options (can move to rival organizations if their current employer deprioritizes simulation). Training standardizers are also beneficiaries (d ≈ 0.25) because they gain the power to define competence through portable, measurable metrics; they have mobile exit (can join another standards body). Field practitioners are the structural targets (d ≈ 0.78) because they must pass simulator thresholds (constrained exit), bear the cost of simulator-field mismatches, and have their professional judgment subordinated to metrics. Operational_safety_culture is abstract but deeply suppressed (identity-locked exit): practitioners internalize that field judgment is secondary, suppressing the long-term development of tacit wisdom that would normally accumulate through incident analysis. Near-miss learning networks are excluded rather than coordinated (they would argue for a different competence pathway, but are structurally prevented from participating in competence-definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification holds because the constraint carries both a genuine coordination function (training people systematically to be ready for catastrophes without waiting for catastrophes to occur) and substantial asymmetric extraction (simulator operators and standardizers gain permanent institutional authority; field practitioners and incident-learning networks lose autonomy; tacit-knowledge development is suppressed). The rising theater ratio (from 0.35 to 0.58) indicates that the constraint is aging toward a piton state: the coordinating function is real but increasingly theatrical, while the extraction persists. If the theater ratio approaches 0.7+, the constraint may reclassify as piton (atrophied coordination, persistent extraction by pure institutional inertia). The suppression measurement (0.55→0.72) shows that increasingly active institutional force is required to prevent field practitioners from reasserting tacit-knowledge pathways. The accessibility collapse (0.68) shows that alternatives are suppressed but not eliminated: organizations with strong near-miss cultures do maintain competence without heavy simulator investment, proving that simulator-sufficiency is not structural necessity. The rise in suppression and theater together is diagnostic of a constraint whose real coordination function is shrinking relative to its extractive function — exactly the tangled-rope-to-piton trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_structural_limit,
    'Is there a structural (not merely technical) limit to how well simulation can replicate the contextual, emotional, cascading-consequence, and decision-under-irreversible-uncertainty dimensions of actual catastrophic events?',
    'Longitudinal comparative study: track organizations using different competence-maintenance models (simulation-primary, near-miss-primary, field-experience-primary) over 20+ years. Measure catastrophe-prevention performance, competence-gap emergence, failure modes in novel scenarios, and tacit-judgment quality in improvisation. If simulator-trained cohorts show divergent failure signatures or competence erosion compared to field-experienced cohorts, a structural fidelity limit exists.',
    'If a structural ceiling exists, the ''simulation_as_sufficient'' reading''s core axiom becomes empirically false even within the reading''s own epistemic framework. The reading would be foreclosed toward ''catastrophe_as_necessary'' or ''near_miss_as_bridge'' — simulation would be necessary but insufficient. If no ceiling, the reading remains defensible. This is the empirical crux of the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_structural_limit, empirical, 'Whether simulation has an irreducible fidelity gap relative to real catastrophic scenarios.').

omega_variable(
    tacit_knowledge_suppression_mechanism,
    'Does the institutional assertion that ''simulator performance is sufficient'' actively suppress the development and transmission of tacit, situation-specific knowledge that would otherwise emerge from field experience and incident analysis?',
    'Ethnographic and cognitive study: compare organizations with simulation-heavy and near-miss-heavy competence models on (a) breadth and depth of incident-analysis networks, (b) practitioners'' investment in field judgment and pattern-recognition development, (c) organizational memory of failure modes and contextual lessons, (d) post-exit practitioner behavior — do people leaving simulator-heavy organizations reassert tacit-knowledge commitment?',
    'If suppression is structural (practitioners internalize that field judgment is secondary, reducing collective investment in near-miss analysis), the long-term competence cost is hidden in the measuring system. Extractiveness would be higher than measured because the constraint extracts not just authority but cognitive capacity and organizational epistemology itself. This would provide evidence for deeper snare-hood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_suppression_mechanism, empirical, 'Whether simulator-sufficiency claims suppress tacit-knowledge development and incident-learning investment.').

omega_variable(
    reading_forecloses_to_near_miss_by_axiom_overriding,
    'As simulator technology matures and theater-ratio rises (simulators become increasingly optimized for certification-passing rather than competence-transfer), does the reading''s own core axiom (''structural equivalence of cognitive demands'') erode? Can the reading hold if simulators diverge from field realism in service of measurability and audit compliance?',
    'Track the evolution of simulator design objectives and actual design choices over time. Measure divergence between field-realism requirements and certification-compliance optimization. If divergence becomes substantial, the reading''s axiom is no longer defensible within its own framework — the reading would have abandoned its core epistemic ground.',
    'The reading could drift from ''simulation is equivalent'' toward ''simulation is an efficient proxy for certification'' — a shift from an empirical claim to an administrative claim. This would constitute axiom_overriding within the reading''s own tradition and would create space for foreclosure toward near_miss_as_bridge (which makes no structural equivalence claim, only sufficiency of field-sourced learning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_forecloses_to_near_miss_by_axiom_overriding, conceptual, 'Whether the reading''s core premise (structural equivalence) can survive institutional pressures that optimize simulators for measurability rather than field-relevance.').

omega_variable(
    practitioner_suppression_internalization,
    'For field practitioners, is suppression of tacit-knowledge development and near-miss learning structural (external institutional barriers) or internalized (practitioners adopt the belief that simulator performance is sufficient)?',
    'Post-exit measurement: practitioners leaving simulator-heavy organizations — do they maintain or reassert commitment to simulator-sufficiency framing after external constraints are removed? If suppression is internalized, the constraint''s epistemic footprint persists after institutional exit; if structural, practitioners reassert field judgment immediately.',
    'If suppression is internalized, the constraint''s actual extraction is deeper than measured — it constrains not just organizational learning but individual epistemic autonomy and professional identity. This would constitute evidence for identity-locking and movement toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_suppression_internalization, empirical, 'Whether practitioner suppression persists after exit — evidence for internalized versus structural suppression.').

omega_variable(
    kernel_reading_coexistence_boundary,
    'Can ''simulation_as_sufficient'' and ''near_miss_as_bridge'' coexist indefinitely as organizational choices, or does institutional rationalization eventually force a kernel-level resolution?',
    'Historical analysis: organizations that have tried to run both pathways simultaneously (heavy simulator investment AND strong incident-learning networks) — do they eventually commit to one, or maintain both? Regulatory pressure analysis: do regulators eventually require one reading or the other, or allow both?',
    'If regulatory or economic pressures eventually force a choice, the kernel moves from ''contested'' to ''resolved'' — one reading forecloses the others. If organizations can sustain both indefinitely, coexistence is stable. This determines whether the kernel contest is a permanent feature of high-reliability organizational structure or a temporary state heading toward resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_boundary, conceptual, 'Whether the three readings can coexist indefinitely or regulatory/economic forces will foreclose some readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 20, 0.54).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 25, 0.57).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.18).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, high_reliability_organizational_learning).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, incident_investigation_institutional_pathways).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'competence_retention_exercise'. The sibling readings 'catastrophe_as_necessary' and 'near_miss_as_bridge' instantiate alternative epistemic frames for the same founding problem. All three readings affect the institutional structure of high-reliability organizations and the mechanisms by which field learning is captured and transmitted. The network links show which constraints' operational success depends on the outcome of the kernel contest. This reading (simulation_as_sufficient) directly competes with near-miss_as_bridge for resource allocation and institutional legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__simulation_as_sufficient, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
