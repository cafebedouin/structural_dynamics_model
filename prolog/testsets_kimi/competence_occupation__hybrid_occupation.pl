% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Hybrid Multi-Mechanism Competence Occupation
 *   domain: organizational/safety
 *
 * SUMMARY:
 *   In high-reliability organizations, the requirement that competence be
 *   occupied through continuous multi-mechanism exerciseâsimulation,
 *   refresher training, procedural reinforcement, and line auditsâhas
 *   become institutionalized despite persistent disagreement about which
 *   mechanisms matter and in what proportion. This constraint is the
 *   hybrid_occupation reading of the competence_occupation kernel; sibling
 *   readings include simulation_sufficiency (simulation alone suffices) and
 *   real_incident_necessity (only actual catastrophic incidents provide
 *   authentic occupation). The hybrid reading treats training optimization as
 *   a perpetual research problem and mandates all mechanisms simultaneously.
 *   Regulators enforce the stack; vendors and researchers benefit from its
 *   perpetuation; frontline operators and operating organizations bear its
 *   costs.
 *
 * KEY AGENTS:
 *   - frontline_operators: Primary target (moderate/constrained) â bears the time burden, fatigue, and cognitive load of perpetual multi-mechanism training.
 *   - operating_organizations: Primary target (powerful/constrained) â bears direct costs, downtime, and compliance overhead without clear marginal safety validation.
 *   - training_infrastructure_vendors: Primary beneficiary (organized/mobile) â collects recurring revenue from the mandated mechanism stack.
 *   - hro_research_community: Primary beneficiary (organized/mobile) â captures grants and career advancement from the perpetual optimization problem.
 *   - regulatory_safety_authorities: Agenda setter (institutional/analytical) â mandates and enforces the hybrid configuration through licensing.
 *   - accident_investigation_boards: Analytical observer (institutional/analytical) â identifies skill decay without prescribing the hybrid architecture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.62).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.48).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Multi-Mechanism Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "organizational/safety").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, 'ffbd827f-0d6e-454e-ba73-08c0720d0235').
narrative_ontology:cs_kernel_codification('ffbd827f-0d6e-454e-ba73-08c0720d0235', formalized).
narrative_ontology:cs_authority_grounding('ffbd827f-0d6e-454e-ba73-08c0720d0235', expertise).
narrative_ontology:cs_interpretation_layer_present('ffbd827f-0d6e-454e-ba73-08c0720d0235').
narrative_ontology:cs_reading_relation('ffbd827f-0d6e-454e-ba73-08c0720d0235', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('ffbd827f-0d6e-454e-ba73-08c0720d0235', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('ffbd827f-0d6e-454e-ba73-08c0720d0235', foundational, no_single_mechanism_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('ffbd827f-0d6e-454e-ba73-08c0720d0235', no_single_mechanism_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('ffbd827f-0d6e-454e-ba73-08c0720d0235', foundational, continuous_optimization_mandatory).
narrative_ontology:cs_axiom_status(continuous_optimization_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('ffbd827f-0d6e-454e-ba73-08c0720d0235', continuous_optimization_mandatory, instrumental).
narrative_ontology:cs_reference_frame('ffbd827f-0d6e-454e-ba73-08c0720d0235', evidence_based_competence_assurance).
narrative_ontology:cs_drift_state('ffbd827f-0d6e-454e-ba73-08c0720d0235', contemporary_hro_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffbd827f-0d6e-454e-ba73-08c0720d0235', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_infrastructure_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, hro_research_community).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operating_organizations).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, competence_decay_avoidance_hypothesis).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, multi_mechanism_reinforcement_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set mandatory recurrent training and audit standards through licensing requirements; update rules based on incident findings and emerging research; verify compliance through inspection and enforcement action.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulatory_safety_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Sell, maintain, and update simulation platforms, refresher courseware, and audit instrumentation under long-term institutional contracts; revenue grows with the number of mandated mechanisms and frequency of exercise.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_infrastructure_vendors, beneficiary,
    organized, biographical, mobile, global).

% Produce peer-reviewed studies on skill decay curves, simulation fidelity, and competence measurement methods; secure grants and career advancement by treating optimal training configuration as an open research question.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, hro_research_community, beneficiary,
    organized, generational, mobile, global).

% Bear direct costs for training systems, staff downtime, and audit preparation; must demonstrate compliance to retain operating licenses; unable to opt for simpler regimes even when internal data suggests diminishing returns.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operating_organizations, payer,
    powerful, biographical, constrained, national).

% Spend significant duty hours in simulations, refresher modules, procedural reviews, and line audits; report training fatigue and task saturation; professional certification depends on satisfying every mandated mechanism.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    moderate, biographical, constrained, national).

% Independently examine incidents and publish findings on contributory factors including skill degradation; their recommendations influence regulatory updates but they do not prescribe specific training architectures.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, accident_investigation_boards, observer,
    institutional, generational, analytical, national).

% Publish evidence that lighter or simulation-focused regimes achieve equivalent safety outcomes at lower burden; their recommendations are cited in academic debate but rarely adopted into binding standards.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, streamlined_training_advocates, excluded,
    moderate, biographical, mobile, global).

narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operator competence in high-consequence environments by enforcing recurrent exercise across simulation, refresher, procedural, and audit channels, preventing skill decay and procedural drift between rare critical events.
% TRANSFER_FUNCTION: Moves time, organizational budget, and cognitive bandwidth from frontline operating activity and core mission output to training infrastructure, compliance documentation, and perpetual research on optimal configuration.
% ABSENT_VOICES: Simulation-sufficiency advocates and resource-constrained operators who would prefer lighter evidence-based regimes; their exclusion is structural because the hybrid mandate closes the policy space for simpler alternatives.
% DISAPPEARANCE_RATIONALE: If the hybrid mandate disappeared, operating organizations would reallocate training budgets, vendors would lose recurring simulation and audit contracts, regulators would need to rewrite licensing frameworks, and accident exposure might shift depending on whether the hybrid stack was net protective or parasitic.
% FOUNDING_PROBLEM: Catastrophic system failures in high-reliability domains traceable to operator skill decay, loss of rare-event fluency, and procedural deviation during critical moments.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards consistently identify skill decay and procedural deviation as contributory factors; however, they do not uniformly corroborate the specific multi-mechanism hybrid configuration as the necessary or optimal response, with some investigations noting training-task saturation as its own risk factor.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the hybrid stack imposes significant time and cost burdens without empirical proof that each additional mechanism prevents marginal accidents. Suppression (0.48) is moderate: the constraint persists through licensing requirements and regulatory inspection, not physical coercion, but alternatives are effectively barred by compliance frameworks. Theater ratio (0.42) reflects growing compliance activity that validates the apparatus more than the competence outcome. Accessibility collapse (0.58) captures the difficulty of validating simpler alternatives when the default is the full hybrid stack. Resistance (0.38) is moderate: frontline operators and some organizations push back on burden, but safety framing dampens organized opposition. The measurement series show extraction and theater accumulating over the interval as the hybrid model became institutionalized without convergence on optimal configuration.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (regulators, vendors, researchers) experience the constraint as a necessary, evidence-based safety architecture with open research questions. The payer seats (frontline operators, operating organizations) experience it as an ever-expanding, incompletely justified burden that consumes operational resources. The engine computes this divergence from the structural asymmetry in exit options and cost-bearing.
 *
 * DIRECTIONALITY LOGIC:
 *   Training infrastructure vendors and the HRO research community are declared beneficiaries: they receive revenue and funding flows generated by the perpetual hybrid mandate. Frontline operators and operating organizations are declared victims: they bear the time and monetary costs without commensurate demonstrated safety returns. Regulators and accident investigation boards sit outside the beneficiary-victim axis but shape the constraint's scope. Directionality for beneficiaries trends toward subsidy (low d); for victims toward extraction (high d); the engine amplifies effective extraction for operators with constrained, certification-dependent exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcatastrophic accidents from skill decayâis genuinely live, which prevents piton classification. The constraint coordinates real safety value (preventing decay) while simultaneously extracting resources through mechanism proliferation (tangled rope). It is not a snare because the coordination function is not cover: competence maintenance is structurally necessary. It is not a rope because the asymmetric burden and lack of consensus on configuration create identifiable victims. Mandatrophy is avoided by requiring both beneficiaries and victims, and by the live founding_problem status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the competence occupation kernel best occupied by continuous multi-mechanism hybrid exercise, pure simulation, or real catastrophic incident exposure?',
    'Comparative longitudinal studies of accident rates and skill retention across organizations adopting each reading; meta-analysis of skill decay curves by mechanism type.',
    'Would determine whether the hybrid constraint is structurally necessary or an overbuilt extractive apparatus; if simulation or incident exposure alone suffices, hybrid reclassifies toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which kernel reading correctly describes necessary competence conditions').

omega_variable(
    perpetual_research_incentive,
    'Does the persistent lack of consensus on optimal training configuration reflect genuine epistemic intractability, or does the training-research complex perpetuate uncertainty to maintain funding streams and regulatory relevance?',
    'Economic analysis of research funding and vendor revenue trajectories correlated with regulatory uncertainty; comparison with domains that achieved training consensus.',
    'If uncertainty is manufactured, the constraint''s extraction is higher than its safety value; if genuine, the perpetual research overhead is a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_research_incentive, empirical, 'Whether training uncertainty is epistemic or instrumental').

omega_variable(
    marginal_safety_value,
    'What is the marginal accident-prevention value of the full hybrid mechanism stack relative to simpler simulation-only or refresher-only regimes?',
    'Controlled natural experiments across jurisdictions or organizations with differing mandate intensities; Bayesian analysis of incident reduction per training dollar.',
    'A near-zero marginal value would indicate the hybrid stack is largely extractive overhead riding on a real but thinner coordination need; high marginal value supports the tangled rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginal_safety_value, empirical, 'Marginal safety return on hybrid training investment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__hybrid_occupation, theater_ratio, 8, 0.26).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__hybrid_occupation, theater_ratio, 16, 0.32).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__hybrid_occupation, theater_ratio, 24, 0.37).
narrative_ontology:measurement(comp_tr_t32, competence_occupation__hybrid_occupation, theater_ratio, 32, 0.4).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__hybrid_occupation, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comp_be_t8, competence_occupation__hybrid_occupation, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(comp_be_t16, competence_occupation__hybrid_occupation, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(comp_be_t24, competence_occupation__hybrid_occupation, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(comp_be_t32, competence_occupation__hybrid_occupation, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(comp_be_t40, competence_occupation__hybrid_occupation, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_occupation__hybrid_occupation, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(comp_su_t16, competence_occupation__hybrid_occupation, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(comp_su_t24, competence_occupation__hybrid_occupation, suppression_requirement, 24, 0.46).
narrative_ontology:measurement(comp_su_t32, competence_occupation__hybrid_occupation, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(comp_su_t40, competence_occupation__hybrid_occupation, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
