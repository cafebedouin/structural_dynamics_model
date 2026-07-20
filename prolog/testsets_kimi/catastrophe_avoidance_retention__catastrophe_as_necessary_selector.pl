% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selector for Organizational Competence
 *   domain: safety engineering / organizational learning / high-reliability systems
 *
 * SUMMARY:
 *   This constraint instantiates the strict catastrophist reading of the
 *   catastrophe_avoidance_retention kernel in safety engineering culture. It
 *   asserts that actual catastrophesâ with their attendant chaos, mortality
 *   salience, and organizational traumaâare the irreducible selection
 *   pressure required to maintain operational competence. The doctrine
 *   delegitimizes high-fidelity simulation and near-miss learning as sources
 *   of false confidence, thereby preserving the epistemic monopoly of
 *   practitioners whose authority is grounded in catastrophe experience. The
 *   constraint functions as an active enforcement structure within
 *   safety-critical industries: simulation programs are defunded, alternative
 *   pedagogies are ridiculed, and the blood price of learning is treated as
 *   inevitable rather than contingent.
 *
 * KEY AGENTS:
 *   - catastrophe_veteran_experts: agenda-setter / beneficiary â irreplaceable authority derived from trauma; low exit because identity is fused to catastrophe experience
 *   - cost_avoidant_leadership: beneficiary â captures avoided simulation spend; high exit via capital mobility
 *   - frontline_technical_operators: payer â bears physical risk; trapped by economic dependency and skill specificity
 *   - affected_public_communities: payer â off-site catastrophe exposure; trapped by geography
 *   - simulation_technology_researchers: excluded â defunded and dismissed; constrained exit to adjacent fields
 *   - external_safety_regulators: observer â analytical seat with limited cultural leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.7).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selector for Organizational Competence").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety engineering / organizational learning / high-reliability systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '42b6823e-fa54-4628-acfa-197a3df6227a').
narrative_ontology:cs_kernel_codification('42b6823e-fa54-4628-acfa-197a3df6227a', distributed).
narrative_ontology:cs_authority_grounding('42b6823e-fa54-4628-acfa-197a3df6227a', practice).
narrative_ontology:cs_interpretation_layer_present('42b6823e-fa54-4628-acfa-197a3df6227a').
narrative_ontology:cs_reading_relation('42b6823e-fa54-4628-acfa-197a3df6227a', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('42b6823e-fa54-4628-acfa-197a3df6227a', catastrophe_avoidance_retention__hybrid_near_miss_learning, coexists_with).
narrative_ontology:cs_axiom('42b6823e-fa54-4628-acfa-197a3df6227a', foundational, actual_catastrophe_necessary_for_competence).
narrative_ontology:cs_axiom_status(actual_catastrophe_necessary_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('42b6823e-fa54-4628-acfa-197a3df6227a', actual_catastrophe_necessary_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('42b6823e-fa54-4628-acfa-197a3df6227a', secondary, simulation_produces_false_confidence).
narrative_ontology:cs_axiom_status(simulation_produces_false_confidence, holdable).
narrative_ontology:cs_axiom_grounding('42b6823e-fa54-4628-acfa-197a3df6227a', simulation_produces_false_confidence, empirically_contingent).
narrative_ontology:cs_reference_frame('42b6823e-fa54-4628-acfa-197a3df6227a', catastrophe_forged_competence).
narrative_ontology:cs_drift_state('42b6823e-fa54-4628-acfa-197a3df6227a', contemporary_peacetime_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('42b6823e-fa54-4628-acfa-197a3df6227a', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_experts).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, cost_avoidant_leadership).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_technical_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, affected_public_communities).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_technology_researchers).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, mortality_salience_theory).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, peacetime_decay_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior safety engineers and incident commanders whose authority and professional identity were forged in major catastrophes. They set organizational safety doctrine, mentor junior staff, and dismiss simulation-based training as producing false confidence. Their career capital and speaking fees depend on irreplaceable catastrophe experience.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_experts, agenda_setter,
    powerful, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_experts, beneficiary).

% Executives and budget controllers who use the doctrine to justify deferred spending on high-fidelity simulation infrastructure and redundant safety systems. They capture the avoided capital expenditure while externalizing the risk of catastrophic failure.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, cost_avoidant_leadership, beneficiary,
    institutional, biographical, arbitrage, national).

% Plant operators, flight crew, maintenance technicians, and emergency responders who work inside hazardous systems. They bear the physical risk that serves as the organization's learning signal; when catastrophes occur, they are the first injured or killed.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_technical_operators, payer,
    powerless, immediate, trapped, local).

% Residential populations and ecosystems adjacent to chemical plants, nuclear facilities, or major infrastructure corridors. They suffer the off-site consequences when an actual catastrophe becomes the selected lesson, with no voice in the safety doctrine that normalizes their exposure.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, affected_public_communities, payer,
    powerless, generational, trapped, local).

% Engineers and cognitive scientists developing high-fidelity simulation, virtual reality training, and near-miss analytics. Their research agendas are structurally defunded and their findings dismissed as academically interesting but operationally invalid by the veteran expert cadre.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_technology_researchers, excluded,
    moderate, biographical, constrained, national).

% Government agencies and international oversight bodies tasked with mandating minimum safety training and incident-prevention standards. They collect fatality statistics, audit training records, and occasionally mandate simulation hours, but struggle to overcome the cultural authority of the catastrophe-experience narrative.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, external_safety_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates organizational attention on high-consequence signals and prevents normalization of deviance by maintaining a direct, visceral causal link between catastrophic failure and collective learning.
% TRANSFER_FUNCTION: Moves the cost of competence maintenance from organizational capital budgets and simulation-infrastructure investment to frontline technical operators and adjacent communities, who pay in blood, mortality, and disruption.
% ABSENT_VOICES: Simulation technology researchers and near-miss analysts are structurally excluded from the safety doctrine; they would argue that equivalent or superior learning is achievable without paying a blood price, but their frameworks are dismissed as producing false confidence and paper credentials.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight, the justification for relying on actual catastrophe as the necessary teacher would collapse. Safety budgets would reallocate toward high-fidelity simulation, immersive rehearsal, and proactive resilience engineering, rearranging the political economy of who possesses legitimate safety expertise.
% FOUNDING_PROBLEM: How to maintain acute safety competence and vigilance in high-hazard organizations during long periods without accidents, when routine and complacency erode risk perception and operational discipline.
% FOUNDING_PROBLEM_CORROBORATION: Disaster sociologists (Perrow, Vaughan) and high-reliability organization scholars attest that routine erodes vigilance. However, resilience-engineering researchers and simulation scientists attest that the founding problem can be solved without catastrophic tuition, corroborating that the problem is live while the proposed solution is contested.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the doctrine externalizes the full cost of organizational learning onto frontline workers and adjacent communities, while the benefits of saved capital and preserved expert status accrue upstream. Suppression (0.70) reflects active delegitimization of simulation and near-miss frameworks; the constraint cannot persist without this enforcement because cheaper, safer alternatives are technically available. Theater ratio (0.45) captures the growing share of safety activity devoted to ritualized catastrophe-posturing and war-story ceremonies that substitute for genuine skill maintenance during peacetime. Accessibility collapse (0.65) measures how thoroughly the doctrine collapses the perceived viability of simulation-based alternatives for true believers. Resistance (0.55) reflects the pushback from simulation researchers, regulators, and some next-generation practitioners.
 *
 * PERSPECTIVAL GAP:
 *   The veteran expert seat experiences the constraint as hard-won wisdom and identity-constituting truth; its directionality is near-beneficiary. The frontline operator seat experiences the same structure as a deferred death sentence whose probability rises as peacetime lengthens; its directionality is near-target. The regulator seat sees a cultural trap that inflates societal risk; its directionality is analytical and symmetric. The engine should compute divergent per-seat types from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (catastrophe_veteran_experts, cost_avoidant_leadership) feed low directionality: their structural relationship to the constraint is subsidization via status preservation and capital avoidance. Victim declarations (frontline_technical_operators, affected_public_communities, simulation_technology_researchers) feed high directionality: they pay the effective extraction in mortality risk, community disruption, and defunded careers. The excluded researcher seat sits near full-target because suppression is aimed directly at their professional existence.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a scaffold because it carries no sunset clause and its justification is the steady state, not a transition. It is not a piton because concentrated beneficiaries (veteran experts and budget guardians) actively capture and maintain the extraction; if they stopped enforcing it, the constraint would weaken. It is not a rope because identifiable victims pay catastrophic costs. The classification as tangled rope preserves the genuine information-filtering functionâactual catastrophes do produce vivid learningâwhile capturing the asymmetric extraction that rides on the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of simulation alternatives structural (budgetary defunding, exclusion from curricula) or internalized (practitioners genuinely cognitively dismiss simulation as invalid regardless of evidence)?',
    'Natural experiment: introduce mandated high-fidelity simulation in a peer organization and measure whether veteran experts'' dismissal persists after objective performance equivalence is demonstrated.',
    'If suppression is primarily internalized, the constraint''s effective suppression exceeds the structural measure because the belief replicates itself through identity fusion even after institutional barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression of simulation alternatives').

omega_variable(
    naturalness_of_peacetime_decay,
    'Is organizational competence decay during long peacetime periods an emergent natural law of human cognition and social systems, or a socially constructed failure of institutional design that could be engineered around?',
    'Cross-industry longitudinal comparison of organizations with equivalent hazard exposure but differing simulation investment levels, measuring incident rates and near-miss detection sensitivity.',
    'If decay is a natural law, the constraint approaches mountain-like inevitability and the victim structure is tragic but not extractive in the engineered sense. If decay is institutionally contingent, the high extraction and active enforcement metrics stand as designed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_peacetime_decay, empirical, 'Whether peacetime decay is natural law or institutional failure').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment system frame most accurately as a practice-based authority (practitioners'' lived experience is the standard) or as an extraction-based authority (institutional hierarchy preserves kernel stability to capture status rents)?',
    'Comparative analysis of whether veteran experts revise their doctrine in the face of empirical simulation-efficacy data (practice grounding) or systematically reject such data to preserve status hierarchy (extraction grounding).',
    'If extraction grounding is more accurate, the authority_grounding should be reclassified to extraction and the interpretation_layer re-evaluated as drift-denial machinery rather than legitimate community interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framing of authority grounding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_avoid_sel_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cat_avoid_sel_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.25).
narrative_ontology:measurement(cat_avoid_sel_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.32).
narrative_ontology:measurement(cat_avoid_sel_tr_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 30, 0.38).
narrative_ontology:measurement(cat_avoid_sel_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.42).
narrative_ontology:measurement(cat_avoid_sel_tr_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(cat_avoid_sel_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cat_avoid_sel_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cat_avoid_sel_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(cat_avoid_sel_be_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cat_avoid_sel_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(cat_avoid_sel_be_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cat_avoid_sel_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cat_avoid_sel_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(cat_avoid_sel_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(cat_avoid_sel_su_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(cat_avoid_sel_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(cat_avoid_sel_su_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_avoidance_retention kernel. The 'catastrophe_as_necessary_selector' reading claims only actual catastrophes suffice; the sibling readings decompose the same kernel into simulation-based and hybrid alternatives. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
