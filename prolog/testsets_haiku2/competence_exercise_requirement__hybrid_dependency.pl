% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Competence Maintenance: Simulation + Real-World Anchoring
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   A global regulatory regime mandates that pilot and flight crew competence
 *   be maintained through a hybrid training model: high-fidelity simulation
 *   exercises combined with periodic real-world anchoring via line operations
 *   and non-jeopardy audits. The regime is justified as necessary to bridge
 *   the gap between simulator fidelity and real-world judgment — pure
 *   simulation creates a fragile equilibrium where crews perform well in
 *   simulators but may falter in operation, while pure real-world exercise is
 *   operationally impossible (every crew training event in revenue operations
 *   risks passenger safety). The hybrid regime appears as tangled_rope from
 *   the payer seats (pilots and crews bear time/cost burdens; airlines bear
 *   infrastructure and scheduling costs) while appearing as rope from the
 *   regulator and training infrastructure seats (legitimate coordination
 *   function, defensible standard). This story instantiates the
 *   hybrid_dependency reading: competence requires BOTH simulation foundation
 *   and periodic real-world anchoring; neither alone is sufficient.
 *
 * KEY AGENTS:
 *   - Flight training infrastructure (institutional agenda-setter, operates simulators and certifies training, benefits from sustained demand for both simulator hours and real-world anchoring time)
 *   - Regulatory certification bodies (institutional agenda-setter, mandates the hybrid standard, benefits from legally defensible compliance criteria)
 *   - Line pilots (moderate-power payer, identity-locked exit, bears time cost and operational friction, argues for lighter real-world requirement)
 *   - Airline operators (powerful payer with constrained exit, bears funding and scheduling costs, also collects litigation-defense benefit)
 *   - Pure-simulation advocates (excluded, argue high-fidelity simulation alone suffices)
 *   - Catastrophe advocates (excluded, argue only real high-stakes exercise maintains true competence)
 *   - Observer safety science (analytical seat, measures actual competence outcomes and incident rates)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.58).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Maintenance: Simulation + Real-World Anchoring").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, 'ce3a0740-6b03-4908-83a8-3c8aa15cf319').
narrative_ontology:cs_kernel_codification('ce3a0740-6b03-4908-83a8-3c8aa15cf319', formalized).
narrative_ontology:cs_authority_grounding('ce3a0740-6b03-4908-83a8-3c8aa15cf319', extraction).
narrative_ontology:cs_interpretation_layer_present('ce3a0740-6b03-4908-83a8-3c8aa15cf319').
narrative_ontology:cs_reading_relation('ce3a0740-6b03-4908-83a8-3c8aa15cf319', competence_exercise_requirement__simulation_as_adequate_exercise, influences).
narrative_ontology:cs_reading_relation('ce3a0740-6b03-4908-83a8-3c8aa15cf319', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_axiom('ce3a0740-6b03-4908-83a8-3c8aa15cf319', foundational, simulation_foundation_necessary).
narrative_ontology:cs_axiom_status(simulation_foundation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ce3a0740-6b03-4908-83a8-3c8aa15cf319', simulation_foundation_necessary, instrumental).
narrative_ontology:cs_axiom('ce3a0740-6b03-4908-83a8-3c8aa15cf319', foundational, real_world_anchoring_necessary).
narrative_ontology:cs_axiom_status(real_world_anchoring_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ce3a0740-6b03-4908-83a8-3c8aa15cf319', real_world_anchoring_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('ce3a0740-6b03-4908-83a8-3c8aa15cf319', hybrid_competence_maintenance_regime).
narrative_ontology:cs_drift_state('ce3a0740-6b03-4908-83a8-3c8aa15cf319', contemporary_simulator_fidelity_advancement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ce3a0740-6b03-4908-83a8-3c8aa15cf319', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flight_training_infrastructure).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, regulatory_certification_bodies).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, line_pilots).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, flight_crews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, airline_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, airline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates certified flight simulators and manages recurrent training curriculum for pilots and flight crews. Designs and enforces the hybrid training standard through curriculum specifications, instructor certification, and training record audits. Collects revenue from simulator time and training program delivery. Justifies the hybrid requirement as ensuring fidelity to real-world operations and meeting regulatory standards. Has leverage to adjust the balance between simulator hours and real-world anchoring time through curriculum design, and can adapt to regulatory changes or technological improvements in simulation fidelity.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flight_training_infrastructure, agenda_setter,
    institutional, generational, arbitrage, global).

% Mandates competence maintenance standards through type certificates and operating regulations (e.g., FAA Part 121, EASA FCL). Sets the requirement that pilots and crews must complete recurrent training at regular intervals, specifying the mix of simulator and real-world operations required. Benefits from the hybrid standard by establishing clear, auditable compliance criteria that reduce regulatory liability — if an incident occurs, the regulator can point to compliance with established standards. Maintains authority to review and revise standards based on safety data, though such revisions occur on decadal timescales. Has low direct cost of the regime but high reputational benefit from a defensible standard.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulatory_certification_bodies, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, regulatory_certification_bodies, beneficiary).

% Must complete recurrent simulator training and periodic non-jeopardy line audits to maintain pilot certification and remain employed. Bears the time cost of recurrent training (typically 1-2 weeks per year in simulator plus audit flights). Experiences training as necessary overhead, though increasingly questions whether the real-world anchoring component (line audits, occasional supervised line operations) adds protective value or is administrative theater. Professional identity is fused with the pilot credential — exiting the regime means loss of career, income, and professional status. Advocates for lighter real-world requirements, arguing that modern high-fidelity simulation plus spot-checks during normal operations should suffice.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, line_pilots, payer,
    moderate, biographical, identity_locked, global).

% Flight engineers, first officers, and cabin crew face the same recurrent training mandate as pilots. Bear equivalent time and identity-lock costs. Have less professional autonomy than captains in curriculum design or timing of training, so constrained exit is even tighter. Compliance is primarily through professional norm and certification requirement, not through active engagement with the competence maintenance rationale.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flight_crews, payer,
    moderate, biographical, identity_locked, global).

% Must fund certified simulator training for all pilots and flight crews and provide operational support for non-jeopardy line audits and supervised line time. Bears the capital cost of simulator contracts, the labor cost of instructor and administrator time, and the operational friction of scheduling crews for training and audit flights (reducing crew availability for revenue operations). Also benefits insofar as compliance with certified standards reduces litigation liability if a safety incident occurs — they can demonstrate that crews were trained to regulatory standards. Exit options are constrained: they cannot operate without regulatory approval, and all approved operators face the same requirement. Has some arbitrage room in scheduling and crew management but cannot avoid the regime.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, airline_operators, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, airline_operators, beneficiary).

% Safety engineers, training specialists, and researchers who argue that modern high-fidelity simulation (full-motion simulators with sophisticated aircraft system models) combined with rigorous scenario-based debriefing constitutes adequate competence exercise. Cite improvements in simulator technology over the past two decades and argue that the real-world anchoring component provides no measurable safety benefit. Advocate for reducing or eliminating line audit requirements to reduce training costs and crew time burden. Are excluded from the standard-setting process because they lack regulatory authority and because their position would dismantle the hybrid regime that training infrastructure depends on. Their alternative framing is published in aviation safety journals and occasionally surfaces in regulatory working groups but does not currently shape policy.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, pure_simulation_advocates, excluded,
    organized, generational, trapped, global).

% Safety researchers and accident investigators who argue that only real catastrophic events (or high-stakes near-misses with genuine jeopardy) provide the irreducible psychological and cognitive exercise needed to maintain true competence. Contend that routine non-jeopardy audits are theater: they lack the emotional resonance and genuine consequence that drive deep learning and retention. Observe that competence often fails not in trained scenarios but in novel, high-stress situations where crews have not experienced true jeopardy. Are excluded from standard-setting because (1) their position is operationally impossible to implement (cannot deliberately create catastrophic events for training), (2) it is ethically undefendable (passenger safety cannot be jeopardized for crew training), and (3) it does not provide a repeatable, auditable regime that regulators need. Their critique lives in accident investigation reports and academic literature but does not shape certification standards.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, catastrophe_advocates, excluded,
    moderate, generational, trapped, global).

% Accumulates empirical evidence on what actually maintains competence in aviation operations. Measures crew performance on simulator-based competence checks, tracks incident and accident rates across training regimes, and analyzes error patterns in line operations. Observes the contest between pure-simulation, hybrid-dependency, and catastrophe-as-anchor framings. Lacks direct authority to mandate standards but publishes research that informs regulatory review cycles. Seat is analytically positioned and has no direct stakes in the outcome, though research careers and professional reputation ride on perceived objectivity.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, observer_safety_science, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, flight_training_infrastructure).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains pilot and crew competence to handle rare, high-stakes scenarios through structured exercise combining high-fidelity simulation (accessible, repeatable, safe, scalable) with periodic real-world anchoring (line operations, non-jeopardy audits, supervised actual aircraft time) to bridge the gap between simulator fidelity and operational reality — pure simulation creates brittle, scenario-specific competence; pure real-world training is operationally impossible.
% TRANSFER_FUNCTION: Extracts time, cost, and operational friction from line pilots and flight crews (who must complete recurrent training cycles) and from airline operators (who must fund training infrastructure and release crews for audit flights). The extracted resources fund simulator facility operations, instructor staffing, curriculum development, and audit program overhead. Regulatory bodies collect the benefit of a legally auditable, universally applicable compliance standard. Training infrastructure collects sustained institutional demand.
% ABSENT_VOICES: Pure-simulation researchers and engineers who argue high-fidelity simulation alone is sufficient are excluded from standard-setting because they lack regulatory authority and their position would dismantle training infrastructure's business model. Catastrophe-theory researchers who argue only real jeopardy exercises true competence are excluded because their position is operationally impossible and ethically indefensible. Industry pilots and flight crews advocating for lighter real-world requirements participate only through airline operators' policy advocacy, not in regulatory working groups.
% DISAPPEARANCE_RATIONALE: If the hybrid requirement disappeared overnight, training regimes would converge on pure simulation (lower cost, easier to schedule, faster crew certification) unless regulators mandated pure real-world anchoring (operationally infeasible). Accident and incident rates would shift within 5-10 years, revealing whether the real-world anchoring component is protective or theater. Regulatory liability exposure would spike because regulators could no longer point to compliance with a certified, universally applicable standard. Airline insurance premiums might increase (higher perceived risk) or decrease (lower training cost offset). The entire structure of competence maintenance would reorganize around whichever reading (simulation-adequate or catastrophe-necessary) proved empirically sustainable.
% FOUNDING_PROBLEM: Early jet-transport training relied primarily on pure simulation, which produced crews who performed well in simulators but failed catastrophically when confronted with rare but real scenarios involving subtle environmental cues, system failures, or high-stress situations that differed from trained profiles (e.g., high-altitude stalls, hydraulic failures in marginal weather). Pure real-world training was infeasible because every training event in revenue operations risked passenger safety. The hybrid regime emerged as an attempt to extract the scalability and safety benefits of simulation while anchoring competence maintenance in periodic contact with operational reality.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and training infrastructure assert the founding problem remains live: crews trained on pure simulation show judgment gaps in real-world operations, and periodic real-world anchoring is necessary to close those gaps. Safety researchers acknowledge historical evidence for this problem but argue the evidence base is now weak: simulator fidelity has improved dramatically, debriefing protocols have evolved, and statistical analysis of modern accident data does not show clear safety advantage for hybrid regimes over high-simulation regimes. Some researchers argue the founding problem was solved 15-20 years ago but the regime persists as institutional inertia. Pure-simulation advocates cite published evidence that high-fidelity simulation with rigorous scenario-based debriefing transfers effectively to real-world performance and argue that real-world anchoring provides no measurable safety margin. Catastrophe researchers counter that neither simulation-only nor routine audits provide the psychological forcing necessary for true competence; only real jeopardy exercises genuine judgment. No external scientific consensus exists; the founding problem is actively contested in research literature and regulatory deliberation.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the regime does solve a real coordination problem (maintaining competence for rare, high-stakes scenarios) while simultaneously extracting costs from payers. The extraction is not pure rent — the coordinating function is genuine — but it is asymmetric: training infrastructure and regulators benefit by maintaining and enforcing the standard, while pilots and crews bear the cost burden. Suppression is moderate (0.42) because the regime is defended by regulatory authority and professional norms, not by overwhelming coercive force; pilots comply primarily through professional identity and certification dependency, not fear. Theater ratio rises over the interval (0.18 to 0.31) as simulator fidelity improves and non-jeopardy audits become more standardized, suggesting that an increasing share of training activity is performative verification of compliance rather than active competence building. The measurement series suggests the regime reached stability around t=25 — extractiveness plateaued, theater stabilized, suppression requirement stopped rising — consistent with an equilibrium where the hybrid requirement is accepted doctrine but its protective value is questioned in research literature.
 *
 * PERSPECTIVAL GAP:
 *   From the training infrastructure and regulatory seats, the hybrid requirement is a legitimate coordination function: crews need structured competence maintenance, simulation alone has known gaps, real-world anchoring addresses those gaps, and the standard is auditable. From the pilot and crew seats (especially identity-locked seats with biographical time horizons), the same structure operates as extracted time burden justified by a coordination story; pilots experience the regime as institutional inertia dressed as safety imperative. From the airline operator seat, the constraint is a compliant-cost-plus-benefit hybrid: they fund it, schedule around it, and also benefit from the liability defense. The engine should compute these differently; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Training infrastructure and regulators are near the beneficiary end (d~0.2): they set the agenda, maintain authority over the standard, and collect the benefit of sustained institutional demand and regulatory clarity. Pilots and crews sit near the target end (d~0.75): they bear time and identity-lock costs, face enforcement via certification dependency, and have low exit options (leaving the regime means leaving the profession). Airlines sit near symmetric (d~0.55): they bear funding and scheduling costs but also collect litigation-defense benefits and have some arbitrage room (scheduling flexibility, influencing pilots' decisions about recurrent timing). The payer seats and beneficiary seats should compute to different constraint types by the engine — payers should experience it as more extractive (snare-adjacent), beneficiaries as coordination (rope-like) — the hybrid classification reflects the mixed structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids pure mandatrophy (exhausted mission) by maintaining a real coordination function: competence maintenance in rare, high-stakes domains IS hard, and pure simulation leaves gaps that real anchoring can fill. However, the regime is vulnerable to a partial mandatrophy critique: simulator fidelity has improved dramatically since the regime was established, debriefing protocols have evolved, and empirical evidence for the gap that justifies the real-world anchoring component is weak or contested. The rising theater_ratio suggests increasing performance of compliance ritual rather than active competence building. The contest between hybrid_dependency, simulation_as_adequate, and catastrophe_as_anchor readings directly addresses this: if simulation fidelity and debriefing have solved the founding problem, the regime is extracting (theater-heavy snare) rather than coordinating (genuine tangled rope). The classification of tangled_rope depends on the evidence for whether the coordination function — bridging the simulator-to-real gap — is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_fidelity_sufficiency,
    'Has simulator fidelity and debriefing protocol advanced to the point where the simulator-to-real competence gap that justified the real-world anchoring component has been eliminated?',
    'Comparative empirical study: match crews trained on high-fidelity simulation + rigorous debriefing against crews trained on hybrid regime; measure performance on novel emergency scenarios (not in curriculum), judgment quality under cognitive load, and error rates on line operations. If the high-simulation group performs equivalently, the gap is closed.',
    'If the gap is closed, the real-world anchoring component becomes theater (pure extraction), and the regime should be reclassified as snare or piton rather than tangled_rope. If the gap persists, tangled_rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_fidelity_sufficiency, empirical, 'Whether modern simulation fidelity eliminates the need for periodic real-world anchoring.').

omega_variable(
    real_world_anchoring_effectiveness,
    'Do non-jeopardy line audits and periodic real aircraft time actually improve competence maintenance, or do they primarily serve as compliance theater?',
    'Longitudinal study tracking incident rates and error patterns for crews stratified by real-world anchoring frequency (holding simulator hours constant). If incident rates are uncorrelated with anchoring frequency, it is theater; if correlated, it is protective.',
    'If theater-dominant, the regime is extractive without protective benefit — snare/piton candidate. If protective, the coordination function is real and tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_world_anchoring_effectiveness, empirical, 'Whether real-world anchoring provides protective benefit or is primarily performative.').

omega_variable(
    simulation_fragility_hypothesis,
    'Is the pure-simulation critique correct that simulator-trained crews develop brittle, scenario-specific competence that fails under novel conditions?',
    'Controlled scenario testing: expose crews (simulator-only vs. hybrid-trained) to emergency scenarios that differ substantially from their training profile; measure performance, decision quality, and error recovery. If brittle failure occurs in simulator-only crews, the hypothesis is supported.',
    'If hypothesis is supported, real-world anchoring addresses a genuine coordination gap and tangled_rope is appropriate. If hypothesis is refuted (simulator-only crews transfer effectively), the real-world anchoring is optional and the regime is rent extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fragility_hypothesis, empirical, 'Whether pure-simulation training produces brittle competence or transfers adequately to novel scenarios.').

omega_variable(
    identity_lock_mechanism_suppression,
    'Is the suppression measured in this constraint primarily structural (regulatory mandates, certification gates) or primarily internalized (pilots'' professional identity fused with compliance)?',
    'Post-exit trajectory study: track pilots who retire or exit the profession; measure whether compliance behaviors persist in non-pilot contexts. If behaviors persist, suppression is partially internalized (identity-locked); if behaviors extinguish, suppression is primarily structural.',
    'If internalized, the effective suppression of pilots'' resistance is higher than the measured structural suppression suggests — the constraint carries the suppression with them. This shifts directionality toward fuller target status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether pilot suppression is structural regulatory mandate or internalized professional identity.').

omega_variable(
    kernel_reading_under_determination,
    'Which sibling reading — hybrid_dependency, simulation_as_adequate, or catastrophe_as_anchor — best matches the actual evidential state of competence maintenance in aviation?',
    'Meta-analysis of accident investigation reports, incident data, simulator-to-line transfer studies, and competence assessment instruments over the past 20 years. Determine which reading''s foundational premise (simulation sufficient / hybrid necessary / catastrophe necessary) is most consistent with observed outcomes.',
    'The reading most consistent with evidence should be the default classification for competence regimes. Other readings become challenger positions. If multiple readings are equiprobable, the regime is genuinely contested and each reading should be authored as a separate constraint story linked via network.affects_constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Which epistemic framing of competence maintenance is most consistent with empirical evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__hybrid_dependency, theater_ratio, 5, 0.21).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__hybrid_dependency, theater_ratio, 10, 0.24).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__hybrid_dependency, theater_ratio, 15, 0.27).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.29).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_requirement__hybrid_dependency, theater_ratio, 25, 0.3).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__hybrid_dependency, theater_ratio, 30, 0.31).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__hybrid_dependency, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comp_be_t25, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(comp_su_t25, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, resource_allocation).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__hybrid_dependency, 0.18).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'competence_exercise_requirement'. The kernel is the fundamental question: what structural arrangement best maintains pilot and crew competence in rare, high-stakes scenarios? Three constraint stories decompose the kernel into three structurally distinct claims: (1) hybrid_dependency (this file) — simulation + real-world anchoring are jointly necessary; (2) simulation_as_adequate — high-fidelity simulation alone is sufficient; (3) catastrophe_as_anchor — only real high-stakes events provide the irreducible exercise. The three stories share the same domain (aviation safety, competence maintenance) and the same referent (what actually maintains competence) but differ in ε (extractiveness of the regime) and in the structural asymmetries they identify. Each story is a separate constraint with its own beneficiaries, victims, and stakeholders. Network edges link them: hybrid_dependency influences both siblings (structurally: the hybrid regime creates pressure on simulation-only regimes to add anchoring, and on catastrophe advocates to show why routine audits are insufficient). The sibling readings coexist across different jurisdictions and institutions but neither forecloses the other within a single framework (each reading's proponents believe their framing of competence maintenance is correct; disagreement is empirical, not logical). See audits/2026-06-11_kernel_decomposition for the full constraint family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
