% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation as Valid Exercise Proxy: Competence Retention via Drill Metrics
 *   domain: safety_engineering/organizational_learning/competence_retention
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   question of whether simulation counts as valid exercise for competence
 *   retention in safety-critical industries (aviation, nuclear operations,
 *   emergency response, maritime). The reading presented here —
 *   simulation_as_proxy — asserts that competence can be adequately retained
 *   and validated through standardized simulation metrics and periodic
 *   drills, without requiring exposure to real catastrophic events. This
 *   reading grounds regulatory compliance frameworks across most OECD safety
 *   systems. However, the constraint exhibits the structural signature of a
 *   false summit (mountain claimed but beneficiaries present), a tangled rope
 *   (coordination + extraction), and a piton (performative certification),
 *   suggesting the reading's legitimacy is contested. The core tension:
 *   regulatory frameworks incentivize simulation-as-proxy because it is
 *   administratively scalable and cost-reducible, but operators, safety
 *   researchers, and accident investigators have systematic doubts about
 *   whether simulation transfer to real performance is sufficient. The
 *   constraint's extractiveness (0.58) reflects that compliance with
 *   simulation mandates is enforced on frontline operators and organizations
 *   despite uncertain safety payoff, while the regulatory compliance
 *   infrastructure and simulation industry benefit from continued
 *   standardized metrics.
 *
 * KEY AGENTS:
 *   - Frontline Operators: Primary victim (powerless/trapped) — careers contingent on simulation scores, but uncertain whether simulations actually prepare them for real catastrophe; bear full risk of readiness failure
 *   - Safety-Critical Organizations: Secondary actor (moderate/constrained) — benefit from standardized metrics and scheduling efficiency; also constrained by regulatory compliance costs; face catastrophe liability despite high simulation scores
 *   - Regulatory Compliance Infrastructure: Primary beneficiary (institutional/arbitrage) — derives legitimacy and operational scalability from simulation-as-proxy metrics; can abandon this reading if political pressure shifts
 *   - Evidence-Based Safety Reform Coalition: Organized agents (organized/mobile) — safety researchers, labor unions, accident investigation boards; see simulation-as-proxy as temporary scaffold with identified sunset (real-time performance monitoring)
 *   - Simulation-Certification Industry: Secondary beneficiary (institutional/arbitrage) — vendors, training providers, assessment companies; maintain apparatus through institutional inertia and regulatory alignment despite weak evidence base
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent regulatory choice as inherent feature of competence science; engine flags as false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.58).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.65).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation as Valid Exercise Proxy: Competence Retention via Drill Metrics").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning/competence_retention").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, 'da5d0ca2-ab52-4724-99ef-cd8cbe06614b').
narrative_ontology:cs_kernel_codification('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', formalized).
narrative_ontology:cs_authority_grounding('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', extraction).
narrative_ontology:cs_interpretation_layer_present('da5d0ca2-ab52-4724-99ef-cd8cbe06614b').
narrative_ontology:cs_reading_relation('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_reading_relation('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', foundational, simulation_transfer_to_real_performance_sufficient).
narrative_ontology:cs_axiom_status(simulation_transfer_to_real_performance_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', simulation_transfer_to_real_performance_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', foundational, standardized_metrics_predict_safety_outcomes).
narrative_ontology:cs_axiom_status(standardized_metrics_predict_safety_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', standardized_metrics_predict_safety_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', competence_validation_through_standardized_metrics).
narrative_ontology:cs_drift_state('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', contemporary_evidence_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('da5d0ca2-ab52-4724-99ef-cd8cbe06614b', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_infrastructure).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, organizational_scheduling_efficiency).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, operational_readiness_integrity).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, actual_catastrophe_prevention_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped in simulation-based competence validation despite knowing it is insufficient. Career advancement and employment contingent on simulation metrics rather than actual readiness. Faces real catastrophe risk while constrained to accept drill-as-proxy framework. Maximum extraction: structured into performative validation while bearing full cost of readiness failure.
constraint_indexing:constraint_classification(competence_exercise_validity__simulation_as_proxy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SAFETY-CRITICAL ORGANIZATION (TANGLED ROPE) — Receives genuine coordination benefit from standardized simulation protocols (enables workforce deployment, scheduling predictability, cross-facility comparisons). Also faces asymmetric extraction: regulatory compliance costs are shifted to simulation metrics rather than real-time competence assurance, and actual catastrophe remains possible despite high simulation scores. Mixed—some agency through metrics optimization, but trapped by regulatory framework.
constraint_indexing:constraint_classification(competence_exercise_validity__simulation_as_proxy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY COMPLIANCE SYSTEM (ROPE) — Pure coordination from this perspective. Simulation metrics provide verifiable, auditable, scalable validation of competence. Enables distributed oversight without requiring real-time operational assessment. Regulatory framework sees the constraint as solving a genuine problem: how to certify competence across thousands of operators and facilities. Beneficiary with full arbitrage — can shift to alternative metrics if needed.
constraint_indexing:constraint_classification(competence_exercise_validity__simulation_as_proxy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EVIDENCE-BASED SAFETY REFORM COALITION (SCAFFOLD) — Organized actors (accident investigation boards, safety research institutes, labor unions) see simulation-as-proxy as a temporary institutional arrangement with a sunset clause. Real-time performance monitoring, incident-triggered retraining, and continuous competence refresh are technical alternatives becoming viable. Coalition recognizes the constraint as a scaffolding: necessary during the era of blunt compliance instruments, but obsolete once granular competence monitoring becomes operationally feasible. Sunset timing: 10-15 years as real-time learning systems mature.
constraint_indexing:constraint_classification(competence_exercise_validity__simulation_as_proxy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SIMULATION-CERTIFICATION INDUSTRY (PITON) — The apparatus persists through institutional inertia despite its primary function (competence retention) being inadequate. The industry benefits from simulation-validation frameworks and maintains them through continued advocacy, vendor lock-in, and regulatory alignment. Actual performance of simulation in preventing catastrophe is weak, but the certification process itself is highly performative and self-perpetuating. Theater ratio high: assessment rituals dominate; real competence tracking is secondary.
constraint_indexing:constraint_classification(competence_exercise_validity__simulation_as_proxy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this reading presents simulation-as-proxy as a natural law of organizational learning: competence cannot be fully retained without regular exercise, simulations provide regular exercise under controlled conditions, therefore simulations ARE sufficient to maintain competence. The logic is presented as inherent to how learning works. However, structural data contradicts the mountain classification — identifiable beneficiaries (compliance infrastructure, scheduling efficiency) and victims (operational readiness) suggest this is a contingent institutional arrangement, not a natural law. Engine will flag as false summit.
constraint_indexing:constraint_classification(competence_exercise_validity__simulation_as_proxy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_exercise_validity__simulation_as_proxy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_exercise_validity__simulation_as_proxy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, TR),
    TR >= 0.70.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Simulation-as-proxy functions as a coordination mechanism — it provides scalable, verifiable, comparable metrics across organizations and operators. However, it also extracts from operators and organizations through enforcement (they must comply despite uncertainty about efficacy) and from the epistemic commons (simulation adequacy becomes assumed rather than tested). The value reflects that the constraint is neither pure coordination nor pure extraction, but a hybrid where coordination benefits accrue to the regulatory system while extraction costs fall on operators and safety integrity. Suppression (0.65): High. Frontline operators face career barriers (advancement contingent on compliance), organizations face regulatory barriers (non-compliance triggers sanctions), and alternative approaches (continuous refresh, real-time monitoring) are suppressed by standardization lock-in and compliance orthodoxy. Theater ratio (0.68): High. Simulation certification is substantially performative — assessment rituals (drills, exams, scoring rubrics) dominate, while actual measurement of whether simulation transfers to real performance is rare and inconsistent. The rise from 0.52 to 0.68 over the interval reflects increasing ritual elaboration as the simulation industry matures, while the evidence base for efficacy grows weaker. Measurements show increasing divergence between compliance theater (rising) and extractiveness (rising but slower), suggesting the constraint is shifting toward piton (performative maintenance) rather than resolving uncertainty.
 *
 * PERSPECTIVAL GAP:
 *   The frontline operator sees a snare: constrained by compliance mandates despite knowing simulation is insufficient. The organization sees tangled rope: genuine coordination benefit from metrics alongside catastrophe risk. The regulator sees rope: pure coordination problem solved. The reform coalition sees a scaffold: temporary solution with identified successor. The industry sees a piton: ritual persisting through inertia. The civilizational analyst sees a false summit: naturalized institutional choice. These gaps reflect real structural differences in who benefits from the constraint versus who bears its costs. The largest gap is between the regulator (rope) and the operator (snare) — they are looking at the same constraint and experiencing fundamentally different classifications because the regulatory framework extracts from operators while benefiting the compliance infrastructure.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs sharply across perspectives because different agents have different structural relationships to the simulation-as-proxy framework. The regulatory compliance system (beneficiary, institutional, arbitrage) experiences low or negative directionality — the framework subsidizes their legitimacy. Frontline operators (victim, powerless, trapped) experience high directionality — they bear extraction. Safety organizations (mixed, moderate, constrained) experience moderate directionality — they benefit from metrics standardization but suffer catastrophe risk. The evidence-based coalition (organized, mobile) experiences low directionality because they can exit to alternative framings and are building successor systems. The simulation industry (beneficiary, institutional, arbitrage) experiences arbitrage directionality — they can pivot if regulatory winds shift. The analytical observer at the civilizational horizon experiences the highest directionality (observer positioned as analyst of the false naturalization) because they see the entire apparatus as contingent institutional choice rather than law of competence science.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that simulation-as-proxy is ONE legitimate reading of the competence validation kernel, but it is being maintained through institutional inertia and regulatory capture, not through empirical vindication. The constraint begins as tangled rope (genuine coordination problem: how to validate competence across distributed operators) but is shifting toward piton (performative ritual maintained by the simulation industry and compliance bureaucracy despite weak evidence base). The empirical challenge is severe: if simulation transfer to real performance is poor, the reading is foreclosed by evidence and only continuous_refresh_hybrid or real_catastrophe_only readings remain structurally valid. The constraint's rising theater ratio (0.52 → 0.68) combined with static-to-rising extractiveness suggests the apparatus is increasingly relying on ritual authority rather than efficacy — a classic sign of piton transition. The mandatrophy resolves by acknowledging that regulatory compliance and actual safety may have decoupled: high simulation compliance (piton theater) coexists with uncertain or degraded actual readiness (snare extraction on operators).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_validity,
    'Do skills learned in simulation transfer to real-catastrophe performance at rates sufficient to prevent harm?',
    'Longitudinal analysis of operator performance in actual emergencies vs simulation scores; correlation between high simulation metrics and low catastrophe rates across organizations; incident investigation data linking competence failures to simulation-trained operators',
    'If transfer rate > 85% reliable: simulation-as-proxy is valid (reading holds). If transfer rate < 65%: simulation is insufficient (foreclosed by empirical evidence). If transfer rate context-dependent: reading coexists with continuous_refresh_hybrid (simulation necessary but not sufficient).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'Transfer validity of simulation training to real emergency performance').

omega_variable(
    regulatory_compliance_vs_actual_safety,
    'Does regulatory compliance via simulation metrics correlate with or decouple from actual catastrophe prevention?',
    'Cross-organizational regression analysis: organizations with highest simulation compliance scores vs actual safety records (near-miss rates, incident frequencies, severity distributions); investigation of organizations with compliance violations vs safety outcomes',
    'If positive correlation persists: simulation-as-proxy is causally adequate (reading holds). If decoupled or inverse correlation: simulation is cover story for extraction (snare foreclosed by evidence). If correlation degrades over time: reading coexists with continuous_refresh_hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_compliance_vs_actual_safety, empirical, 'Correlation between regulatory compliance and actual catastrophe prevention').

omega_variable(
    kernel_reading_contest,
    'Is competence validation via simulation a legitimate reading of the competence_exercise_validity kernel, or is it a false naturalization of a contingent institutional arrangement?',
    'Committer-axis analysis: what foundational claim (axiom) distinguishes this reading from its siblings? Is the axiom holdable (live claim in competence science) or overridden (empirically or formally superseded)? Does the authority grounding (regulatory compliance infrastructure) depend on simulation-as-proxy being true, or does it depend on simulation-as-proxy being believed (extraction axis)?',
    'If axiom is empirically overridden: reading forecloses to evidence and coexists only as a degraded institutional position (piton). If axiom remains holdable but coexists with evidence for continuous_refresh_hybrid: reading influences rather than forecloses sibling. If authority grounding is extraction-dependent: reading is maintained through institutional inertia despite empirical challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether simulation-as-proxy is a legitimate or naturalized kernel reading').

omega_variable(
    performance_decay_under_simulation_only,
    'At what interval does competence decay below operational safety thresholds if maintained only through simulation without real-emergency practice?',
    'Controlled study of operator cohorts trained via simulation-only vs continuous refresh with emergency exposures; measurement of performance degradation across 1, 3, 5, 10 year intervals; incident analysis in organizations with long periods without real emergency exposure',
    'If decay is minimal and slow (threshold > 5 years): simulation-as-proxy is adequate. If decay is rapid (threshold < 2 years): continuous_refresh_hybrid is structurally necessary. If decay is steep post-threshold: reading coexists with hybrid but foreclosed as sole strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_decay_under_simulation_only, empirical, 'Competence decay interval under simulation-only maintenance').

omega_variable(
    organizational_motivation_alignment,
    'Does the regulatory framework incentivize simulation-as-proxy because it is effective, or because it is administratively convenient and cost-reducing?',
    'Policy analysis: cost-benefit models underlying simulation-validation mandates; comparison of regulatory burden (cost to comply with simulation mandates) vs burden of continuous-refresh alternatives; investigation of regulatory capture by simulation vendors or compliance bureaucracies',
    'If motivated by efficacy evidence: reading is driven by structural truth. If motivated by administrative convenience: reading is maintained by extraction logic despite empirical challenge. Either way affects how the constraint is classified over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_motivation_alignment, conceptual, 'Whether regulatory framework''s simulation preference is efficacy-driven or administratively convenient').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compex_sim_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(compex_sim_tr_t5, competence_exercise_validity__simulation_as_proxy, theater_ratio, 5, 0.62).
narrative_ontology:measurement(compex_sim_tr_t10, competence_exercise_validity__simulation_as_proxy, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(compex_sim_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(compex_sim_be_t5, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(compex_sim_be_t10, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(compex_sim_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(compex_sim_su_t5, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(compex_sim_su_t10, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, regulatory_capture_in_safety_standards).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, organizational_learning_measurement_validity).

% DUAL FORMULATION NOTE:
% The competence_exercise_validity kernel decomposes into three structurally distinct constraint stories, each representing a different reading of whether simulation counts as valid exercise. This story (simulation_as_proxy) has ε ≈ 0.58 and claims tangled_rope. The continuous_refresh_hybrid reading asserts simulation is necessary but insufficient (likely ε ≈ 0.48, tangled_rope or snare depending on continuous-refresh feasibility). The real_catastrophe_only reading asserts simulation is insufficient substitute (likely ε ≈ 0.72, snare). Each reading has its own beneficiary/victim structure, its own perspectives, and its own temporal measurements. The network edges show that this reading influences and coexists with its siblings rather than foreclosing them — all three readings are simultaneously held by different parties in contemporary safety governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
