% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the simulation_fidelity_threshold reading of the
 *   catastrophe_proxy_sufficiency kernel. The constraint frames operator
 *   competence retention as dependent on simulation technology crossing a
 *   sufficiency threshold where stress and uncertainty match real
 *   catastrophic events. This reading asserts that adequate simulation is
 *   possible — that fidelity can be engineered to the point where
 *   simulation-trained operators remain genuinely competent — and that the
 *   organizing principle for training regulation should be technology-driven
 *   fidelity standards. The founding problem (how to train for catastrophe
 *   without uncontrolled catastrophe) is treated as SOLVABLE via investment
 *   in better simulation. Sibling readings contest this: the
 *   catastrophe_necessity_reading argues only actual catastrophe maintains
 *   true competence; the hybrid_degradation_reading argues simulation
 *   maintains procedures but tacit knowledge degrades generationally; the
 *   simulation_as_proxy_catastrophe_reading treats simulation as already
 *   sufficient and questions the endless raising of fidelity thresholds. This
 *   reading sits in the middle: threshold-dependent but not infinitely
 *   perfectible, technology-enabled but not category-complete.
 *
 * KEY AGENTS:
 *   - simulation_technology_vendors — organized beneficiaries; capture market expansion through threshold escalation
 *   - operating_organizations (airlines, nuclear, emergency response) — institutional payers; bear equipment and training costs; constrained exit due to regulatory mandate
 *   - front_line_operators (pilots, reactor operators, incident commanders) — moderate-power payers with identity_locked exit; invest time and cognitive burden; know their competence depends on artificial substitutes
 *   - certification_authorities (FAA, NRC, national regulators) — institutional agenda setters; set fidelity standards; face legal liability if simulation-trained operators fail in real catastrophe
 *   - catastrophe survivors and families — excluded powerless seats; have strongest empirical knowledge of operator failure modes but are absent from threshold setting
 *   - competing competence frameworks — research and practitioner voices arguing for hybrid, experiential, or generational approaches; marginalized in formal certification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.62).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.48).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold for Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'b82c2059-9249-4e0b-993e-6f840ff04f7c').
narrative_ontology:cs_kernel_codification('b82c2059-9249-4e0b-993e-6f840ff04f7c', formalized).
narrative_ontology:cs_authority_grounding('b82c2059-9249-4e0b-993e-6f840ff04f7c', extraction).
narrative_ontology:cs_interpretation_layer_present('b82c2059-9249-4e0b-993e-6f840ff04f7c').
narrative_ontology:cs_reading_relation('b82c2059-9249-4e0b-993e-6f840ff04f7c', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b82c2059-9249-4e0b-993e-6f840ff04f7c', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b82c2059-9249-4e0b-993e-6f840ff04f7c', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, influences).
narrative_ontology:cs_axiom('b82c2059-9249-4e0b-993e-6f840ff04f7c', foundational, fidelity_threshold_sufficiency).
narrative_ontology:cs_axiom_status(fidelity_threshold_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('b82c2059-9249-4e0b-993e-6f840ff04f7c', fidelity_threshold_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('b82c2059-9249-4e0b-993e-6f840ff04f7c', foundational, technology_dependent_threshold).
narrative_ontology:cs_axiom_status(technology_dependent_threshold, holdable).
narrative_ontology:cs_axiom_grounding('b82c2059-9249-4e0b-993e-6f840ff04f7c', technology_dependent_threshold, instrumental).
narrative_ontology:cs_reference_frame('b82c2059-9249-4e0b-993e-6f840ff04f7c', simulation_as_sufficient_substitute).
narrative_ontology:cs_drift_state('b82c2059-9249-4e0b-993e-6f840ff04f7c', contemporary_post_accident_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b82c2059-9249-4e0b-993e-6f840ff04f7c', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_infrastructure_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_bodies_enforcing_standards).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness climbs from 0.38 to 0.62 over the interval, then plateaus. The initial state (t=0) reflects a constraint still seen as fundamentally coordinative: simulation training is presented as solving a genuine collective-action problem. Over time (t=0 to t=30), extractiveness grows as vendors systematically raise fidelity thresholds, operating organizations face repeated equipment upgrades with unclear competence gains, and certification standards become technology-dependent rather than outcome-dependent. Plateau at t=30–40 reflects the constraint reaching a stable rent-seeking level: the threshold-setting process has institutionalized, alternative competence frameworks are successfully suppressed in formal training, and the beneficiary has captured the standard-setting authority. Theater ratio rises from 0.18 to 0.41 — indicating that an increasing share of the constraint's enforcement activity is performative (simulator validation studies, fidelity benchmarks, technology demonstrations) rather than directly maintaining competence. At t=30+, theater stabilizes near 0.41 because the theatrical component reaches its effective level: enough theater to maintain vendor credibility without triggering organized resistance. Suppression is moderate (0.48 at end) because resistance is real and organized (research communities, some operators, post-accident investigators), but successfully marginalized rather than eliminated. The coercion grid shows that accessibility collapse and stakes inflation increase across all four levels, but most sharply at the structural level (how certification systems are organized around fidelity standards); resistance decays especially at the organizational and class levels (large operators and the professional cohort gradually accept the threshold) while structural resistance holds at 0.50 — competing frameworks and survivor testimony never entirely disappear.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (certification authority) and the vendor should experience this constraint as coordination: they have jointly established a binding standard, they both benefit from its stability, and they present it as solving the founding problem. Operators at the organizational level experience it as partly coordinative (they do get competence maintenance) and partly extractive (costs exceed benefits; alternatives are closed). Front-line operators and researchers outside the vendor-regulator axis experience it as nearer to pure extraction: they bear the burden, their alternative competence theories are suppressed, and the threshold's adequacy is unverified. The engine should compute divergent types at different seats: rope/coordination at the agenda-setter and vendor axes; snare or tangled_rope at the payer axes. The claim/metric independence is deliberate here: I claim rope (the reading's core assertion) while the metrics describe moderately extractive operation with rising theater, which the engine will translate to snare or tangled_rope at victim seats — that divergence is precisely the kernel contest that sibling readings turn on.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation_technology_vendors are full beneficiaries (d near 0.0): they collect growing revenue from equipment sales, licensing, and standards validation. They have arbitrage-level exit options (they can sell to other markets if this one constricts). Certification authorities sit near symmetric (d near 0.5): they benefit from a stable, technically defensible standard that shields them from liability; they also bear the cost of maintaining the standard as technology evolves. Operating organizations are targets (d near 0.7–0.8): they pay for equipment, training, and personnel time; constrained exit (regulatory mandate). Front-line operators are deep targets (d near 0.85): identity_locked exit (professional identity depends on certification and operational role); they internalize the constraint as part of being a competent operator; suppression is partially internalized (they believe the training is necessary, even as they doubt its adequacy). Excluded stakeholders (survivors) have no exit option at all within the formal system (d approaches 1.0, but they are outside the scope). No overrides are needed — the structural derivation produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy dynamics: the founding problem (how to train for catastrophe without creating catastrophe) is live, but the founding solution (fidelity thresholds as competence surrogates) is increasingly contested. The hybrid_degradation_reading and catastrophe_necessity_reading both argue that the constraint's mandate — to maintain genuine competence — is being eroded by the technological focus on fidelity metrics. Post-accident investigations consistently reveal operator competence failures that simulation did not predict. However, the constraint persists because: (1) the alternative (allowing real catastrophes for training) is ethically intolerable; (2) the threshold framework, though contested, is institutionalized in certification systems; (3) the vendor-regulator axis successfully suppresses alternative competence frameworks. This is a piton-candidate (mask worn over a failing mandate) OR a tangled_rope (genuine coordination function + asymmetric extraction). The theater ratio rising to 0.41 suggests piton dynamics (increasingly theatrical maintenance), but the founding_problem_status=live and disappearance_verdict=world_rearranges suggest the mandate is not dead, merely contested. I classify as rope with piton-trajectory warning: the constraint coordinates training infrastructure, but that coordination is wearing a false sufficiency narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_sufficiency_empirical_gap,
    'Does simulation training at current fidelity thresholds actually prevent competence failures in real catastrophic events, or are current failures unrelated to fidelity deficits?',
    'Systematic post-accident analysis correlating operator training regime (simulation fidelity level) with failure modes. Large-N database of operators trained at different fidelity levels experiencing real incidents, correlated with outcome.',
    'If failures correlate with fidelity deficits, the reading is validated — simulation investment does improve competence and the threshold is meaningful. If failures occur despite high-fidelity training (e.g., stress-response collapse, tacit knowledge gaps), the catastrophe_necessity_reading is validated and this reading becomes snare (false sufficiency narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_sufficiency_empirical_gap, empirical, 'Whether the fidelity threshold actually predicts competence maintenance or merely provides theatrical validation.').

omega_variable(
    generational_competence_atrophy,
    'Does competence in stress-response and tacit decision-making degrade across generations of operators trained solely on simulation, even with fidelity thresholds crossed?',
    'Longitudinal cohort study comparing operators trained during decades of high-fidelity simulation with operators trained during eras of real incident exposure or mixed real/simulated training. Measure stress-response speed, error recovery, and rare-event decision quality.',
    'If generational atrophy is detected even with adequate fidelity, the hybrid_degradation_reading is validated and this reading''s claim of sufficiency is overridden. If no atrophy is detected, the fidelity-threshold reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_competence_atrophy, empirical, 'Whether simulation fidelity can substitute for generational knowledge transmission in catastrophe-response competence.').

omega_variable(
    threshold_escalation_game,
    'Is the continuous raising of fidelity thresholds a technical necessity (real competence gaps emerging as fidelity improves) or a market-capture mechanism (vendors and regulators colluding to expand the equipment market)?',
    'Analysis of threshold-setting processes: do thresholds rise because new fidelity measurements reveal previous inadequacy, or because vendors propose new capabilities and regulators adopt them without empirical competence validation? Examine regulatory meeting minutes, vendor marketing, and competence validation studies.',
    'If escalation is technical necessity, the reading''s ε is lower and coordination is genuine. If escalation is market-capture, extractiveness is higher and the constraint approaches snare (pure fidelity-escalation extraction with competence validation as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_escalation_game, conceptual, 'Whether fidelity thresholds reflect competence science or vendor-regulator capture of training standards.').

omega_variable(
    internalized_suppression_mechanism,
    'Is the suppression of alternative competence frameworks (experiential, hybrid, generational) structural (regulatory exclusion) or internalized (operators and researchers have come to believe simulation-fidelity is the only valid approach)?',
    'Survey operators, training designers, and researchers on their actual beliefs about competence adequacy and alternative training methods. Then measure post-exit persistence: do researchers who leave fields dominated by fidelity-threshold requirements adopt alternative frameworks in adjacent domains, or do they maintain the fidelity-focused belief system?',
    'If internalized, suppression is higher than the 0.48 scalar suggests — operators carry the constraint belief with them after professional exit, limiting the emergence of alternative competence theories. If structural, suppression could be reduced by regulatory reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Whether suppression of alternative competence frameworks is enforced externally or internalized in professional identity.').

omega_variable(
    committer_kernel_contest_structure,
    'Which sibling reading (catastrophe_necessity, hybrid_degradation, simulation_as_proxy_catastrophe) is empirically validated by operational outcomes?',
    'This omega IS the kernel contest itself. Resolution requires large-N comparative analysis of competence outcomes under different training regimes. The constraint story is the fidelity_threshold reading; the container for the full contest is the narrative_ontology module''s kernel tracking mechanism.',
    'Each sibling reading instantiates a different constraint with different ε, different beneficiary structure, and different type. This omega names the contest without resolving it — that resolution belongs to the corpus-level measurement of post-accident operator performance and the success/failure trajectory of certification regimes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_contest_structure, conceptual, 'The kernel contest: can simulation fidelity substitute for real catastrophe in competence maintenance, and if so, at what threshold?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.18).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 5, 0.22).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 10, 0.27).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 15, 0.32).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 20, 0.37).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 25, 0.4).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 30, 0.41).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t35, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 35, 0.41).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_tr_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t35, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 35, 0.62).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_be_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t35, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 35, 0.48).
narrative_ontology:measurement(catastrophe_proxy_sufficiency_su_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.18).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_proxy_sufficiency kernel. All four readings contest the same question: can simulation training substitute for real catastrophic events in maintaining operator competence? The simulation_fidelity_threshold reading asserts YES-if-threshold. The sibling readings contest this from different angles: catastrophe_necessity_reading (NO, only real catastrophe works), hybrid_degradation_reading (PARTIAL, procedural maintenance but tacit knowledge degrades), simulation_as_proxy_catastrophe_reading (YES, and the problem is endless threshold escalation). Each reading is a separate constraint with its own ε, beneficiary structure, and type. They are linked via this network edge: the fidelity_threshold reading INFLUENCES the other three by establishing the institutional framework within which they must operate, but does NOT foreclose any of them logically. All four readings remain live positions held by different actor coalitions (vendors+regulators, researchers, practitioners, alternative-framework advocates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
