% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: High-Fidelity Simulation as Proxy Catastrophe for Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This constraint instantiates the reading that high-fidelity simulation is
 *   functionally equivalent to real catastrophic events for maintaining
 *   operator competence in high-reliability systems. Under this doctrine,
 *   scheduled drills and simulator sessions become the legitimate and
 *   sufficient mechanism for catastrophe avoidance retention, displacing the
 *   older view that only actual failure provides adequate selection pressure.
 *   The constraint operates as an institutionalized coordination
 *   mechanismâregulators mandate it, operators comply, vendors supply
 *   itâwhile asymmetrically transferring catastrophic tail risk to
 *   frontline workers and the public, who suffer the consequences if the
 *   equivalence claim is false. As one reading of the contested
 *   catastrophe_avoidance_retention kernel, it competes with
 *   catastrophe-as-necessary-selector and hybrid-near-miss-learning
 *   frameworks.
 *
 * KEY AGENTS:
 *   - safety_regulators (agenda_setter, institutional/analytical): Set simulation standards and benefit from an inspectable compliance metric.
 *   - high_reliability_operators (beneficiary, institutional/constrained): Meet regulatory bars via simulation, substituting scheduled drills for deeper systemic safety investment.
 *   - simulation_vendors (beneficiary, organized/mobile): Capture revenue from the institutionalized demand for simulator infrastructure.
 *   - frontline_operators (payer, moderate/identity_locked): Bear career and safety risk if simulation inadequacy leaves them unprepared for real chaos.
 *   - public_exposed (payer, powerless/trapped): Bear catastrophic tail risk with no voice in fidelity standards.
 *   - safety_scientists (observer, analytical): Empirically test the equivalence claim but struggle against institutional momentum.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.58).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.52).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "High-Fidelity Simulation as Proxy Catastrophe for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'ccab8c0f-aca7-40f2-a01c-766113148957').
narrative_ontology:cs_kernel_codification('ccab8c0f-aca7-40f2-a01c-766113148957', distributed).
narrative_ontology:cs_authority_grounding('ccab8c0f-aca7-40f2-a01c-766113148957', expertise).
narrative_ontology:cs_interpretation_layer_present('ccab8c0f-aca7-40f2-a01c-766113148957').
narrative_ontology:cs_reading_relation('ccab8c0f-aca7-40f2-a01c-766113148957', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('ccab8c0f-aca7-40f2-a01c-766113148957', catastrophe_avoidance_retention__hybrid_near_miss_learning, coexists_with).
narrative_ontology:cs_axiom('ccab8c0f-aca7-40f2-a01c-766113148957', foundational, high_fidelity_simulation_is_genuine_practice).
narrative_ontology:cs_axiom_status(high_fidelity_simulation_is_genuine_practice, holdable).
narrative_ontology:cs_axiom_grounding('ccab8c0f-aca7-40f2-a01c-766113148957', high_fidelity_simulation_is_genuine_practice, empirically_contingent).
narrative_ontology:cs_axiom('ccab8c0f-aca7-40f2-a01c-766113148957', foundational, scheduled_drill_sufficiency_for_competence_retention).
narrative_ontology:cs_axiom_status(scheduled_drill_sufficiency_for_competence_retention, holdable).
narrative_ontology:cs_axiom_grounding('ccab8c0f-aca7-40f2-a01c-766113148957', scheduled_drill_sufficiency_for_competence_retention, empirically_contingent).
narrative_ontology:cs_reference_frame('ccab8c0f-aca7-40f2-a01c-766113148957', simulation_centered_competence).
narrative_ontology:cs_drift_state('ccab8c0f-aca7-40f2-a01c-766113148957', contemporary_regulatory_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ccab8c0f-aca7-40f2-a01c-766113148957', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, high_reliability_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_vendors).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, public_exposed).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, competence_without_catastrophe_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and audit mandatory simulation-hour requirements and fidelity standards for operator certification. They benefit from having an inspectable, standardized competence metric that scales across jurisdictions without requiring actual failure events.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Nuclear, aviation, and chemical plant operators who maintain licenses by demonstrating simulation compliance. They avoid the catastrophic cost of real incidents while meeting regulatory bars, but may substitute simulation depth for systemic safety investment.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, high_reliability_operators, beneficiary,
    institutional, generational, constrained, global).

% Companies that design and sell high-fidelity simulators, scenario libraries, and drill certification programs. Revenue scales with regulatory mandate and operator dependence on scheduled simulation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Pilots, control-room operators, and emergency responders who must recurrently certify in simulators to retain qualified status. They bear the time burden and career risk if simulation inadequacy leaves them unprepared for the social and physiological chaos of real catastrophes.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Communities near high-risk facilities and transportation passengers who depend on operator competence but have no voice in simulator fidelity standards or scenario design. They bear catastrophic tail risk if the equivalence claim is false.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, public_exposed, payer,
    powerless, generational, trapped, local).

% Researchers studying learning transfer from simulated to real emergencies. Their empirical findings sometimes support and sometimes challenge the equivalence claim, but their voice is diluted against institutional momentum.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains rare-event operational competence across distributed high-reliability organizations without waiting for actual catastrophic failures; provides a scalable, inspectable, and repeatable training standard that regulators can verify.
% TRANSFER_FUNCTION: Moves the burden of competence demonstration from catastrophe exposure to scheduled simulation; moves catastrophic tail risk from organizations and regulators to frontline workers and the public, who bear consequences if simulation inadequacy is exposed only in real failure.
% ABSENT_VOICES: Disaster sociologists who argue that mortality salience and organizational trauma are irreplaceable learning mechanisms; frontline workers with lived experience that drills lack real panic and ambiguity; communities near plants who are never consulted on simulator fidelity standards.
% DISAPPEARANCE_RATIONALE: If the equivalence doctrine vanished, regulators would lose their primary inspectable competence metric and need alternative frameworks; operators would face pressure for deeper systemic redundancy or acceptance of higher incident rates; the simulation industry would contract; and competing competence models such as foreign-incident analysis and near-miss learning would gain institutional traction.
% FOUNDING_PROBLEM: Catastrophic failures are too rare and destructive to serve as routine competence maintenance for high-reliability operators; organizations need a repeatable, safe method to prepare for high-consequence events without experiencing them.
% FOUNDING_PROBLEM_CORROBORATION: Safety scientists outside the simulation vendor community corroborate the rarity problem but empirically dispute whether simulation solves it; disaster researchers attest that real catastrophe dynamics involve social, political, and physiological factors absent from controlled simulators.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the doctrine permits organizations to claim competence and externalize tail risk without proven catastrophe-level readiness. Suppression (0.52) reflects the institutional marginalization of alternative frameworksâparticularly the view that only real catastrophes teachâvia regulatory standardization and professional consensus. Theater ratio (0.42) captures the drift of many simulation programs toward checkbox compliance, where scenario design lags actual incident profiles and repetition replaces adaptive learning. Accessibility collapse (0.65) is substantial because, once the equivalence doctrine is accepted, advocating for catastrophe exposure or hybrid models becomes professionally untenable. Resistance (0.38) is present but fragmented among disaster researchers and frontline workers lacking institutional power. The temporal series show extraction and theater accumulating over forty years as simulation mandates expanded and enforcement hardened, while suppression rose as alternatives were institutionally closed off.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (regulators, operators, vendors) experience the constraint as genuine coordination: it solves the scarcity of catastrophes for training and provides scalable, inspectable standards. The payer seats (frontline operators, public) experience the same structure as risk imposition: they bear the consequences of an unproven equivalence claim. The engine should compute strong seat divergence, with directionality near the beneficiary pole for regulators and near the target pole for the public.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared explicitly: regulators gain an enforceable metric, operators gain compliance without catastrophe costs, vendors gain revenue. Victims are frontline operators (identity-locked to certification requirements) and the public (trapped in spatial proximity to risk). The structural derivation should place safety_regulators and high_reliability_operators at low d, simulation_vendors at low-to-moderate d, frontline_operators at high d, and public_exposed at very high d. No override is needed because the beneficiary/victim declarations and exit options (analytical vs. trapped/identity_locked) naturally produce this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction. The genuine coordination functionâmaintaining skills without actual deathsâis real and necessary, which prevents snare classification. However, the asymmetric risk transfer to non-consenting payers and the accumulation of theater over time prevent pure rope classification. The founding problem (catastrophes are too rare for training) is contested because empirical evidence on transfer validity is mixed; the constraint persists partly because it serves institutional convenience, not solely because it solves the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_validity,
    'Does high-fidelity simulation actually produce equivalent competence outcomes to real catastrophe exposure under chaotic, high-stakes conditions?',
    'Longitudinal field studies comparing incident outcomes between organizations relying primarily on simulation versus those with recent real catastrophe experience; physiological and cognitive load measurement during real versus simulated emergencies.',
    'If negative, the constraint is more extractive than coordinated, raising effective extraction for public and frontline payer seats; if positive, the coordination claim strengthens and the asymmetric risk transfer is mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'Whether simulated emergencies transfer to real catastrophe performance.').

omega_variable(
    practice_drift_vs_doctrine,
    'Has actual simulation practice drifted into theatrical compliance, and does the divergence between claimed and actual fidelity change the constraint classification?',
    'Audit simulator scenarios against historical incident profiles; measure scenario update latency; survey frontline operators on perceived realism and adaptive learning versus ritual repetition.',
    'High drift would indicate the constraint is becoming a piton or snareâmaintained performatively despite atrophied functionârather than a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_drift_vs_doctrine, empirical, 'Gap between claimed simulation fidelity and actual drill practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of catastrophe-required and hybrid doctrines structural (funding and career systems) or internalized (professional identity fusion with simulation-centered safety culture)?',
    'Track publication and funding rates for dissenting safety research; survey safety engineers on identity and resistance to catastrophe-as-teacher frameworks; observe whether dissent persists after exit from specific institutions.',
    'If internalized, effective suppression exceeds the structural measureâthe constraint is self-policing through professional identity even where formal barriers are low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternative safety doctrines.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sim_proxy_cat_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sim_proxy_cat_tr_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 8, 0.22).
narrative_ontology:measurement(sim_proxy_cat_tr_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 16, 0.3).
narrative_ontology:measurement(sim_proxy_cat_tr_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 24, 0.36).
narrative_ontology:measurement(sim_proxy_cat_tr_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 32, 0.4).
narrative_ontology:measurement(sim_proxy_cat_tr_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(sim_proxy_cat_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sim_proxy_cat_be_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(sim_proxy_cat_be_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(sim_proxy_cat_be_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(sim_proxy_cat_be_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(sim_proxy_cat_be_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sim_proxy_cat_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sim_proxy_cat_su_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(sim_proxy_cat_su_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(sim_proxy_cat_su_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(sim_proxy_cat_su_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 32, 0.51).
narrative_ontology:measurement(sim_proxy_cat_su_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_avoidance_retention kernel. It is linked to sibling readings catastrophe_as_necessary_selector and hybrid_near_miss_learning as a constraint family. The kernel decomposes because the structurally distinct claims (simulation equivalence, catastrophe necessity, hybrid portfolio) have different epsilon values, beneficiary structures, and empirical statuses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
