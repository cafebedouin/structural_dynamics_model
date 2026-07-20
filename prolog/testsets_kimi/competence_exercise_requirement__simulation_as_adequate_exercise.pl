% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulation as Adequate Exercise of Competence Kernel
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   In high-reliability organizations, the claim that scheduled high-fidelity
 *   simulation plus structured debriefing constitutes fully adequate exercise
 *   of the competence kernel has become a regulatory standard. Operating
 *   organizations adopt it for cost predictability; simulator vendors scale
 *   their markets around it; frontline operators are certified through it.
 *   The constraint coordinates training at scale while simultaneously
 *   transferring catastrophic tail risk to the public and compressing
 *   real-world skill maintenance into a cheaper, schedulable format. It is
 *   one reading of a contested kernel, structurally incompatible with
 *   readings that demand real catastrophe or hybrid anchoring.
 *
 * KEY AGENTS:
 *   - standards_setting_body (agenda_setter, institutional/constrained): codifies training adequacy rules and accepts simulation-based compliance
 *   - operating_organizations (beneficiary, powerful/constrained): realize cost savings by substituting simulation for live training and catastrophic exposure
 *   - simulation_vendors (beneficiary, organized/mobile): supply high-fidelity simulators and debriefing platforms
 *   - frontline_operators (payer, moderate/identity_locked): certified via simulation cycles; bear skill-atrophy risk and liability exposure
 *   - public_at_risk (payer, powerless/trapped): unknowingly carries catastrophic tail risk if simulated competence proves insufficient
 *   - hybrid_model_advocates (excluded, moderate/constrained): argue for real-world anchoring but are sidelined in standards panels
 *   - accident_investigation_boards (observer, institutional/analytical): incident reviewers who intermittently question simulation adequacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.62).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.45).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulation as Adequate Exercise of Competence Kernel").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, 'c2c02373-4b63-4911-8f83-29c1187037f4').
narrative_ontology:cs_kernel_codification('c2c02373-4b63-4911-8f83-29c1187037f4', formalized).
narrative_ontology:cs_authority_grounding('c2c02373-4b63-4911-8f83-29c1187037f4', expertise).
narrative_ontology:cs_interpretation_layer_present('c2c02373-4b63-4911-8f83-29c1187037f4').
narrative_ontology:cs_reading_relation('c2c02373-4b63-4911-8f83-29c1187037f4', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('c2c02373-4b63-4911-8f83-29c1187037f4', competence_exercise_requirement__hybrid_dependency, forecloses).
narrative_ontology:cs_axiom('c2c02373-4b63-4911-8f83-29c1187037f4', foundational, simulation_exercise_sufficiency).
narrative_ontology:cs_axiom_status(simulation_exercise_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('c2c02373-4b63-4911-8f83-29c1187037f4', simulation_exercise_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('c2c02373-4b63-4911-8f83-29c1187037f4', foundational, competence_without_contingent_exposure).
narrative_ontology:cs_axiom_status(competence_without_contingent_exposure, holdable).
narrative_ontology:cs_axiom_grounding('c2c02373-4b63-4911-8f83-29c1187037f4', competence_without_contingent_exposure, empirically_contingent).
narrative_ontology:cs_reference_frame('c2c02373-4b63-4911-8f83-29c1187037f4', scheduled_simulation_excellence).
narrative_ontology:cs_drift_state('c2c02373-4b63-4911-8f83-29c1187037f4', contemporary_high_reliability_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c2c02373-4b63-4911-8f83-29c1187037f4', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, operating_organizations).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_vendors).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, public_at_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies training adequacy standards and accepts scheduled simulation with debriefing as compliant exercise of the competence kernel. Sets certification thresholds measured in simulator hours and protocol adherence. Can revise the standard but faces institutional pressure to maintain regulatory stability and industry compliance costs.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, standards_setting_body, agenda_setter,
    institutional, generational, constrained, national).

% Airlines, nuclear utilities, and high-risk operators that substitute simulator-based training for live operational exposure or hybrid apprenticeship. They realize substantial cost avoidance and scheduling predictability while remaining compliant. Their exit is constrained by regulatory mandates and competitive cost structures.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, operating_organizations, beneficiary,
    powerful, biographical, constrained, national).

% Develop and sell high-fidelity simulators, scenario libraries, and debriefing platforms. Revenue scales with regulatory acceptance of simulation-only adequacy. They market fidelity improvements as closing the reality gap.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Pilots, control-room operators, and surgical teams whose licenses depend on simulator-based certification cycles. They may experience tacit skill decay or negative transfer but cannot unilaterally demand real-world anchoring without jeopardizing employment and professional standing. Their professional identity is fused to the certification path.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators, payer,
    moderate, biographical, identity_locked, national).

% General public and adjacent communities who depend on HRO performance but cannot opt out of airspace, grid proximity, or medical services. They bear catastrophic tail risk if simulated competence proves insufficient in rare but consequential events.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, public_at_risk, payer,
    powerless, generational, trapped, national).

% Safety researchers, veteran operators, and union safety officers who argue that simulation is insufficient without periodic real-world anchoring or live-fire validation. They are structurally underrepresented on standards panels where compliance metrics are defined by simulator vendors and operating organizations.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, hybrid_model_advocates, excluded,
    moderate, generational, constrained, national).

% Review incidents and near-misses for competence failures. They intermittently question whether simulation adequately prepared operators for the event dynamics observed, but lack regulatory authority to mandate alternative training regimes.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, accident_investigation_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, operating_organizations).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, schedulable, and standardized mechanism for high-reliability organizations to maintain operator competence for rare, high-consequence scenarios without relying on stochastic catastrophic events or prohibitively expensive continuous live operations.
% TRANSFER_FUNCTION: Moves the cost and burden of competence maintenance from operating organizations and training budgets to frontline operators and the public, who bear the latent risk of competence gaps manifesting in catastrophic failure.
% ABSENT_VOICES: Proponents of catastrophe-dependent or hybrid competence modelsâexperienced operators who sense tacit skill decay, and safety researchers documenting simulator-to-reality transfer failuresâare structurally underrepresented on standards panels where adequacy is measured by simulator hours and debrief logs.
% DISAPPEARANCE_RATIONALE: If the adequacy claim vanished overnight, operating organizations would face immediate pressure to fund substantially more expensive live-training, apprenticeship, or catastrophe-exposure programs; regulatory compliance frameworks would require rewriting; the simulation industry's revenue model would contract; and frontline operators would demand revised certification paths anchored in real-world performance.
% FOUNDING_PROBLEM: High-risk industries needed to maintain operator competence for rare, high-consequence scenarios without relying solely on the stochastic occurrence of actual catastrophes or maintaining prohibitively expensive continuous live operations.
% FOUNDING_PROBLEM_CORROBORATION: Operating organizations and simulation vendors attest the problem remains live and that simulation solves it. Accident investigation boards and independent safety researchers outside the benefiting parties attest the problem is partially solved but the current arrangement overclaims adequacy; empirical studies of skill decay and negative transfer are corroborated by regulatory dissent filings and union safety reports.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the constraint systematically transfers the cost of competence maintenance from organizations to operators and the public, while the adequacy claim insulates organizations from bearing fuller training costs. Suppression (0.45) reflects institutional marginalization of hybrid and catastrophe-anchor models in regulatory discourse rather than overt prohibition. Theater ratio (0.40) captures the performative dimension of debriefing protocols and fidelity claims that increasingly substitute for empirical transfer validation. Accessibility collapse (0.60) because alternatives such as mandatory live-fire regimes or catastrophe-dependent training are institutionally unimaginable within the current compliance framework. Resistance (0.30) is moderate but diffuse, arising from frontline intuition and occasional accident reports rather than organized opposition. Measurements share one time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as genuine coordination: scalable, safe, standardized training that solves a real collective-action problem in high-risk industries. The payer seatsâfrontline operators and the publicâexperience the same structure as risk transfer that conceals competence decay behind compliance theater. The engine computes this divergence from the structural asymmetry in exit options and cost-bearing; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating organizations and simulation vendors sit near the beneficiary pole: they collect cost savings and revenue from the constraint's operation. Standards_setting_body sits near symmetricâit exercises authority without directly capturing the extracted value. Frontline_operators and public_at_risk sit near the target pole: they bear the costs of competence gaps and catastrophic risk without corresponding bargaining power or exit. Hybrid_model_advocates are excluded and receive no subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both beneficiaries (cost-saving organizations, simulator vendors) and victims (operators, public). Without the victim declaration, the story might read as a Ropeâgenuine coordination of training resources. Without the beneficiary declaration, it might read as a Mountainâthe inevitable form of modern training technology. The Tangled Rope classification captures that simulation genuinely coordinates training logistics while simultaneously extracting risk-bearing capacity from those excluded from the adequacy determination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_ceiling,
    'Is there an irreducible gap between simulated and real-world competence that debriefing cannot close, and does that gap grow with time since last real-world anchoring?',
    'Comparative performance studies and incident analyses contrasting simulator-only operators against hybrid-trained peers in actual critical events, augmented by longitudinal skill-decay metrics.',
    'If a persistent irreducible gap exists, the constraint''s extractiveness is higher than authored because the adequacy claim systematically overstates competence transfer; if debriefing fully closes the gap, the coordination function dominates and the constraint shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_ceiling, empirical, 'Irreducible fidelity gap between simulation and real-world competence').

omega_variable(
    catastrophe_rate_validation,
    'Do long catastrophe-free intervals validate simulation adequacy, or do they merely reflect layered system redundancy and luck masking progressive competence decay?',
    'Statistical analysis of near-miss rates, latent error detection trends, and micro-failure frequencies against historical baselines pre-dating simulation-dominant regimes.',
    'If competence decay is hidden by redundancy, the constraint is more extractive than it appears; if competence is truly maintained, the coordination claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_rate_validation, conceptual, 'Whether catastrophe-free operation validates simulation or masks decay').

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the competence_exercise_requirement kernel; how would adopting a sibling reading restructure the beneficiary-victim map and extraction profile?',
    'Comparative institutional analysis across jurisdictions or industries that have adopted hybrid or catastrophe-anchor training models, measuring cost distribution and incident outcomes.',
    'If hybrid_dependency is adopted, this constraint''s victim set shrinks and it may degrade to scaffold or piton; if catastrophe_as_necessary_anchor is correct, this constraint functions as a snare extracting safety margin from the public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Structural contingency of this reading relative to sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sim_adeq_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sim_adeq_tr_t8, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 8, 0.25).
narrative_ontology:measurement(sim_adeq_tr_t16, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 16, 0.3).
narrative_ontology:measurement(sim_adeq_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.35).
narrative_ontology:measurement(sim_adeq_tr_t32, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 32, 0.38).
narrative_ontology:measurement(sim_adeq_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(sim_adeq_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sim_adeq_be_t8, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(sim_adeq_be_t16, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(sim_adeq_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(sim_adeq_be_t32, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(sim_adeq_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sim_adeq_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sim_adeq_su_t8, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(sim_adeq_su_t16, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(sim_adeq_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(sim_adeq_su_t32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(sim_adeq_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, resource_allocation).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_requirement kernel. The three readingsâsimulation_as_adequate_exercise, catastrophe_as_necessary_anchor, and hybrid_dependencyâare structurally distinct claims about how competence is maintained. They form a constraint family linked by mutual logical exclusion or pressure, and are modeled as separate stories to preserve Îµ-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
