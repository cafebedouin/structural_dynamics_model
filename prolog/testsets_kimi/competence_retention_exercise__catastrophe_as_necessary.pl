% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe as Necessary for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In safety engineering and high-reliability organization studies, a
 *   contested doctrine holds that only actual catastrophic events provide the
 *   organizational learning and visceral stakes required to maintain genuine
 *   competence, while simulation is merely rehearsal. This doctrine functions
 *   as an institutional constraint on resource allocation and risk
 *   acceptance. It is one reading of the competence_retention_exercise
 *   kernel, which asks how organizations retain catastrophe-avoidance
 *   competence. The sibling readings treat high-fidelity simulation or
 *   near-miss analysis as sufficient. This reading extracts by normalizing
 *   catastrophe as a necessary system reset, thereby justifying reduced
 *   investment in prevention and suppressing alternative learning models.
 *
 * KEY AGENTS:
 *   - organizational_leadership: Primary agenda-setter and beneficiary (institutional/constrained) â sets the doctrine and captures budget savings
 *   - operations_management: Secondary beneficiary (powerful/mobile) â gains operational flexibility from reduced prevention overhead
 *   - frontline_operators: Primary payer and victim (powerless/constrained) â bears direct physical risk under diminished prevention
 *   - affected_public: Secondary payer and victim (powerless/trapped) â bears externalized catastrophe risk
 *   - simulation_advocates: Excluded voice (moderate/constrained) â structurally dismissed from competence definition
 *   - safety_regulators: Analytical observer (institutional/analytical) â investigates but cannot override doctrinal commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.68).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.62).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe as Necessary for Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, 'a5e811d4-12f5-470d-89f0-e188652f460b').
narrative_ontology:cs_kernel_codification('a5e811d4-12f5-470d-89f0-e188652f460b', distributed).
narrative_ontology:cs_authority_grounding('a5e811d4-12f5-470d-89f0-e188652f460b', expertise).
narrative_ontology:cs_interpretation_layer_present('a5e811d4-12f5-470d-89f0-e188652f460b').
narrative_ontology:cs_reading_relation('a5e811d4-12f5-470d-89f0-e188652f460b', competence_retention_exercise__simulation_as_sufficient, coexists_with).
narrative_ontology:cs_reading_relation('a5e811d4-12f5-470d-89f0-e188652f460b', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('a5e811d4-12f5-470d-89f0-e188652f460b', foundational, catastrophe_required_for_genuine_competence).
narrative_ontology:cs_axiom_status(catastrophe_required_for_genuine_competence, holdable).
narrative_ontology:cs_axiom_grounding('a5e811d4-12f5-470d-89f0-e188652f460b', catastrophe_required_for_genuine_competence, empirically_contingent).
narrative_ontology:cs_axiom('a5e811d4-12f5-470d-89f0-e188652f460b', foundational, simulation_produces_structurally_inadequate_stakes).
narrative_ontology:cs_axiom_status(simulation_produces_structurally_inadequate_stakes, holdable).
narrative_ontology:cs_axiom_grounding('a5e811d4-12f5-470d-89f0-e188652f460b', simulation_produces_structurally_inadequate_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('a5e811d4-12f5-470d-89f0-e188652f460b', catastrophe_driven_competence).
narrative_ontology:cs_drift_state('a5e811d4-12f5-470d-89f0-e188652f460b', contemporary_safety_science, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a5e811d4-12f5-470d-89f0-e188652f460b', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, organizational_leadership).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, operations_management).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, affected_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets organizational safety policy and capital allocation, using the doctrine that only catastrophes produce genuine competence to justify lower spending on simulation, preventive redundancy, and near-miss programs. Captures budgetary savings and reduced operational overhead. Exit is constrained by shareholder return expectations and competitive pressure.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, organizational_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced preventive maintenance and training interruptions. Can meet production targets more easily when prevention budgets are compressed because the doctrine normalizes catastrophe as inevitable and educational. Mobile across firms but operates within the same industry norms.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, operations_management, beneficiary,
    powerful, biographical, mobile, national).

% Work inside the socio-technical system under reduced preventive investment justified by the doctrine. Bear the direct physical risk of catastrophic failure that the doctrine treats as necessary for organizational learning. Exit is constrained by labor-market boundaries and skill specificity.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators, payer,
    powerless, immediate, constrained, local).

% Live or work near safety-critical facilities whose operators accept higher catastrophic risk under the doctrine. Bear externalized health, environmental, and property risk. Exit is trapped by geography, housing markets, and lack of regulatory recourse.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, affected_public, payer,
    powerless, immediate, trapped, regional).

% Researchers and technologists who develop high-fidelity simulation and near-miss analysis tools. Structurally excluded from the competence-definition conversation because the doctrine dismisses their outputs as rehearsal without visceral stakes. Their professional standing depends on acceptance of the doctrine they oppose.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_advocates, excluded,
    moderate, biographical, constrained, national).

% Mandate prevention and investigate catastrophic failures. They collect evidence on whether simulation and near-miss programs produce equivalent competence but lack authority to override the doctrinal commitments of regulated organizations.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, organizational_leadership).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates organizational understanding of why safety competence decays during extended calm periods and provides a shared framework for post-catastrophe investigation, restructuring, and budget reallocation.
% TRANSFER_FUNCTION: Moves organizational resources and risk burden away from continuous prevention, simulation, and near-miss analysis toward post-catastrophe response capacity; transfers catastrophe risk from organizational budgets to frontline workers and the surrounding public.
% ABSENT_VOICES: Simulation technology vendors, near-miss analysis specialists, and high-fidelity training advocates are structurally excluded from the competence definition; they would argue that genuine competence can be maintained without catastrophic failures but are dismissed by the doctrine's claim that only real events produce visceral stakes.
% DISAPPEARANCE_RATIONALE: If the doctrine disappeared, organizations would reallocate safety resources toward prevention and simulation, budgetary justifications for risk acceptance would collapse, institutional tolerance for catastrophic failures would decline, and the careers built on post-catastrophe investigation would shrink.
% FOUNDING_PROBLEM: Safety-critical organizations experience irreversible competence decay and normalization of deviance during extended incident-free periods, losing the visceral awareness and urgent attention required for high-reliability operations.
% FOUNDING_PROBLEM_CORROBORATION: Safety researchers studying normalization of deviance attest that calm periods correlate with complacency, but simulation researchers and near-miss analysis advocates contest that catastrophes are the only remedy; corroboration exists from outside the immediate beneficiary set but is actively disputed by excluded parties.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the doctrine decouples resource allocation from prevention efficacy, transferring risk to workers and the public. Suppression is substantial (0.62) because the doctrine actively dismisses simulation and near-miss alternatives as insufficient, collapsing their conceptual accessibility. Theater ratio is moderate (0.40): post-catastrophe investigations are partly genuine learning but increasingly performative, producing lengthy reports without structural reform. Accessibility collapse is high (0.75) because once the doctrine is institutionalized, prevention-first alternatives become unthinkable. Resistance is moderate (0.58) because competing schools in safety science actively contest the premise. The temporal series show gradual drift upward as the doctrine has become more entrenched and budget pressure has intensified over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as a realistic framework for managing finite safety budgets and maintaining vigilance, while the payer seats experience it as a justification for endangering their lives and communities. The engine computes this divergence from the structural data: identical catastrophic risk produces opposite classifications depending on whether the agent collects savings or bears harm.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizational leadership and operations management are structural beneficiaries: they collect budget savings and operational flexibility, giving them low directionality. Frontline operators and the affected public are structural targets: they bear the physical and financial risk of catastrophes that the doctrine renders acceptable, giving them high directionality. Simulation advocates are excluded rather than coordinated; their exclusion is the enforcement mechanism that keeps the doctrine stable. Safety regulators occupy an analytical seat with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the doctrine as pure coordination (rope) by insisting on named victims and asymmetric extraction, and prevents mislabeling it as pure extraction (snare) by acknowledging the genuine coordination function of post-catastrophe learning. The temporal drift measurements capture the risk that the coordination function is being progressively overtaken by budget extraction, which would push the constraint toward snare over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validity_catastrophe_learning,
    'Does empirical evidence in safety science actually support the claim that competence retention requires catastrophic events, or does near-miss and simulation data show equivalent learning outcomes?',
    'Meta-analysis of organizational learning studies comparing catastrophe-driven versus simulation-driven versus near-miss-driven competence retention in high-reliability organizations.',
    'If empirically unsupported, this reading functions primarily as extraction; if supported, the coordination function is genuine and the tangled rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validity_catastrophe_learning, empirical, 'Empirical basis for catastrophe-dependent learning').

omega_variable(
    budget_extraction_vs_genuine_doctrine,
    'Is the catastrophe-necessary doctrine primarily adopted because it accurately describes learning dynamics, or because it justifies budgetary savings on prevention and simulation?',
    'Comparative organizational study tracking budget allocation changes before and after adoption of the doctrine, controlling for actual incident rates and industry segment.',
    'If adoption correlates with budget cuts rather than empirical conviction, the beneficiary structure reveals extraction rather than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(budget_extraction_vs_genuine_doctrine, empirical, 'Budgetary motive versus empirical belief').

omega_variable(
    kernel_reading_contested_ground,
    'This constraint is one reading of the competence_retention_exercise kernel. The sibling readings assign different structural roles to non-catastrophic events. Which reading an organization adopts determines whether prevention budgets are legitimate or wasted.',
    'Not single-resolvable; requires separate constraint stories for each reading linked via network relationships.',
    'The classification depends on treating the catastrophe-as-necessary claim as a stable commitment. If the kernel is better modeled as one constraint with observable-dependent classification, the epsilon-invariance principle would be violated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contested_ground, conceptual, 'Kernel decomposition and reading stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cre_catastrophe_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cre_catastrophe_tr_t8, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 8, 0.26).
narrative_ontology:measurement(cre_catastrophe_tr_t16, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 16, 0.3).
narrative_ontology:measurement(cre_catastrophe_tr_t24, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 24, 0.34).
narrative_ontology:measurement(cre_catastrophe_tr_t32, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 32, 0.37).
narrative_ontology:measurement(cre_catastrophe_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(cre_catastrophe_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cre_catastrophe_be_t8, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(cre_catastrophe_be_t16, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(cre_catastrophe_be_t24, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(cre_catastrophe_be_t32, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(cre_catastrophe_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cre_catastrophe_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(cre_catastrophe_su_t8, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(cre_catastrophe_su_t16, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(cre_catastrophe_su_t24, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(cre_catastrophe_su_t32, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(cre_catastrophe_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, resource_allocation).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_retention_exercise kernel. The kernel decomposes into three structurally distinct claims about how organizations retain catastrophe-avoidance competence. This reading treats real catastrophic events as strictly necessary, producing high extraction from at-risk populations. Sibling readings treat simulation or near-misses as sufficient, with different beneficiary and victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
