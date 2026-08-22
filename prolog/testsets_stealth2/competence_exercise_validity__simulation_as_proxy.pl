% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation-as-Proxy Competence Validation Regime
 *   domain: organizational/safety-engineering
 *
 * SUMMARY:
 *   In regulated hazardous operations — nuclear generation, aviation, process
 *   industry, emergency medicine — the operative rule is that scheduled
 *   simulation counts as valid exercise of rare-event competence: simulator
 *   sessions, tabletop drills, and announced scenario exercises produce the
 *   metrics by which operators, regulators, and insurers establish that
 *   readiness is maintained. Exercise validity is documented, audited, and
 *   closed out as compliance; the absence of adverse events is then cited as
 *   proof the regime works. The arrangement solves a real problem —
 *   catastrophes cannot be rehearsed live — while transferring the cost of
 *   the gap between drilled and demanded competence onto those exposed to
 *   real events. KEY AGENTS (by structural relationship): -
 *   operator_senior_management: agenda-setter (powerful/mobile) — sets
 *   exercise depth, collects the budget relief, rotates out before gaps
 *   surface - regulatory_oversight_bodies: agenda-setter and collector
 *   (institutional/constrained) — defines valid evidence, audits documents,
 *   carries tail blame - training_simulation_industry: beneficiary
 *   (organized/arbitrage) — sells the regime its own evidence -
 *   frontline_operators: primary bearer (moderate/identity_locked) — drills
 *   the scenarios, meets the un-drilled event - hazard_adjacent_public:
 *   residual bearer (powerless/trapped) — holds the tail risk, holds no seat
 *   - internal_safety_engineers: inside observer (moderate/constrained) —
 *   designs and scores the exercises, doubts the transfer -
 *   incident_investigation_boards: analytical observer
 *   (institutional/analytical) — documents the gap after the fact
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.62).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.52).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation-as-Proxy Competence Validation Regime").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "organizational/safety-engineering").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, 'd52c48ad-36d1-4a56-a119-7f8c5e5cc443').
narrative_ontology:cs_kernel_codification('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', formalized).
narrative_ontology:cs_authority_grounding('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', expertise).
narrative_ontology:cs_interpretation_layer_present('d52c48ad-36d1-4a56-a119-7f8c5e5cc443').
narrative_ontology:cs_reading_relation('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', competence_exercise_validity__real_catastrophe_only, forecloses).
narrative_ontology:cs_reading_relation('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', competence_exercise_validity__continuous_refresh_hybrid, forecloses).
narrative_ontology:cs_axiom('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', foundational, proxy_drills_are_valid_competence_exercise).
narrative_ontology:cs_axiom_status(proxy_drills_are_valid_competence_exercise, holdable).
narrative_ontology:cs_axiom_grounding('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', proxy_drills_are_valid_competence_exercise, empirically_contingent).
narrative_ontology:cs_axiom('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', secondary, compliance_validation_suffices_for_readiness).
narrative_ontology:cs_axiom_status(compliance_validation_suffices_for_readiness, holdable).
narrative_ontology:cs_axiom_grounding('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', compliance_validation_suffices_for_readiness, conventional).
narrative_ontology:cs_reference_frame('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', compliance_grade_simulation_adequacy).
narrative_ontology:cs_drift_state('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', post_severe_accident_investigations, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d52c48ad-36d1-4a56-a119-7f8c5e5cc443', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, operator_senior_management).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_oversight_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, training_simulation_industry).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, hazard_adjacent_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, internal_safety_engineers).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, simulation_transfer_sufficiency_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, compliance_as_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define which exercise formats satisfy competence-validation requirements and accept simulator logs, drill rosters, and scenario scorecards as the evidence of record. Audit against documentation rather than observing unannounced performance. Demanding continuous full-scale real-condition exercises would multiply oversight cost and political friction, so the documentable-metric model is the oversight bargain they administer. They collect low-cost verifiable compliance and carry reputational exposure when real events outrun the drilled scenarios.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulatory_oversight_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, regulatory_oversight_bodies, beneficiary).

% Set the exercise calendar, choose scenario families and difficulty, and schedule drills to minimize production disruption. Report completion and pass-rate metrics upward and to regulators. Budget headroom preserved by shallow exercise programs and audit-clean records figure in advancement; the executives who set exercise depth typically rotate to other posts before any readiness gap surfaces in a real event.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, operator_senior_management, agenda_setter,
    powerful, biographical, mobile, national).

% Sell simulator platforms, scenario libraries, instructor certification, and compliance documentation services. Revenue scales with mandated exercise hours and renewal cycles; product roadmaps follow the criteria regulators accept as valid evidence. Industry associations promote standards that specify simulatable objectives.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, training_simulation_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Staff the control rooms, cockpits, wards, and response teams. Complete scheduled drills against known scenario families and are scored on them; licensure and recertification run through the same exercise regime. Declining a drill or disputing its validity is a career-ending act. Off the record they report that real events ask for combinations and durations the scenario library never rehearsed; their professional self-concept as trained-for-this is built on the certificates the regime issues.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, frontline_operators, payer,
    moderate, biographical, identity_locked, national).

% Live downwind, downstream, or on approach paths near the facilities. Bear the consequences when real events exceed what was rehearsed. Hold no seat in scenario selection or exercise-depth decisions and typically learn where the gaps were only from post-incident reports.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, hazard_adjacent_public, payer,
    powerless, generational, trapped, regional).

% Design scenarios, run evaluator checklists, and sign the exercise records. Many accumulate private doubt about transfer fidelity — the gap between checklist performance and unscripted demand — but their assessments feed the same compliance file, and pressing the doubt formally risks standing in a small industry. They see the whole structure from inside and rarely hold the pen that changes it.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, internal_safety_engineers, observer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, internal_safety_engineers, payer).

% Convene after failures and reconstruct which competencies the event demanded versus which the exercise history covered. Their findings periodically document the drilled-versus-demanded gap and recommend deeper or unannounced exercises; the recommendations return to the same regulatory and budget machinery that produced the original exercise plan.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, incident_investigation_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__simulation_as_proxy, operator_senior_management).
narrative_ontology:fixing_cost_class(competence_exercise_validity__simulation_as_proxy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives hazardous operations a repeatable, schedulable way to rehearse rare-event procedures, team roles, and equipment handling without inflicting the event; produces a common auditable record that lets multiple shifts, contractors, and agencies align on the same emergency playbook.
% TRANSFER_FUNCTION: Moves exercise funding from operating budgets to simulation vendors and internal training departments; moves assurance upward — completion metrics from crews to management to regulators to the public record — while moving residual readiness risk downward onto the people exposed to real events.
% ABSENT_VOICES: Hazard-adjacent communities and advocates of unannounced, full-scale, cross-boundary exercises are absent from scenario-selection tables; exercise depth is set by the operators and regulators whose costs and oversight burdens scale with depth. Those bearing the tail risk do not choose what gets rehearsed.
% DISAPPEARANCE_RATIONALE: If the rule that simulation counts as valid exercise vanished overnight, every license pathway, audit protocol, insurance rating, and exercise contract keyed to simulation metrics would need renegotiation; operators would face either far costlier continuous real-condition exercises or explicit admission that readiness is unvalidated; the compliance-documentation industry would lose its product.
% FOUNDING_PROBLEM: After mid-century industrial, aviation, and reactor accidents, regulators needed a way to require rehearsal of rare catastrophic procedures without waiting for real events or harming anyone during training — scheduled simulation was built as the safe proxy that would keep rare-event competence occupied between events.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: accident investigation board reports attest both that the founding problem was real and that it remains live, and the academic human-factors literature on skill decay independently attests the persistence of the rehearsal problem. No source outside the beneficiary set attests that compliance-grade frequency and depth suffice — that claim is attested only by regulators, management, and vendors inside the arrangement.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial but bounded: the regime performs real rehearsal work, and the extraction is the spread between what compliance-grade exercise costs and what event-grade readiness would demand, booked as budget relief and legitimacy. Suppression (0.52) is institutional closure rather than force — deeper alternatives (unannounced full-scale exercises, cross-boundary stress tests) remain visible but are priced and scheduled out of reach, and dissent is penalized through career machinery rather than sanction. Theater (0.46) is high but not dominant: announced scenarios, rehearsed evaluators, and checklist scoring mean close to half of drill activity demonstrates compliance rather than stresses competence, while genuine procedural and team-skills transfer still occurs. Accessibility collapse (0.5): once the equivalence of simulation and exercise is institutionally fixed, deeper alternatives collapse to paper options. Resistance (0.42): safety engineers, veteran operators, and post-accident boards press for realism and lose to budget logic in ordinary years, winning only episodic reforms after visible failures. The measurement series share one seven-point grid (1979-2024) so every metric is authored at every examined time point; trajectories are monotonic drift, not cyclical — the audit-and-recertification ratchet hardened enforcement alongside rising extraction and theater. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine (by directionality and scope). Identity-lock dynamics bind the frontline seat: licensure, recertification, and the self-concept of being trained-for-this are issued by the regime itself, so exit is identity_locked rather than merely constrained.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the management seat the arrangement is an assurance system it built and can defend line-by-line to auditors; from the frontline seat the same calendar is choreographed performance whose certificates fuse with professional identity — an operator cannot dispute drill validity without disputing their own qualification. Regulators occupy a dual position: the metric regime makes oversight cheap and legible (pulling them toward the beneficiary end), while post-event blame lands on them (pushing toward the target end). The public experiences none of the machinery and all of the residual risk. If the identity frame broke — if certificates came to be seen as recording compliance rather than readiness — frontline operators would convert from quiescent payers into the arrangement's principal resistance, and the resistance and suppression profiles would move together.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: management, regulators, and vendors sit at the beneficiary end; operators and the public at the target end. Two overrides correct the derivation where structural data alone misplaces a seat. First, regulatory_oversight_bodies derive near-full beneficiary (d approximately 0.15) from their declared benefit, but they carry real tail exposure — post-accident blame and mandate scrutiny — so d is overridden to 0.32. Second, frontline_operators derive near-full target from victim status compounded by identity lock, but they are also the regime's direct daily beneficiaries — rehearsed procedures and team coordination genuinely serve them — so d is overridden down to 0.68 rather than resting at the trapped-target extreme. hazard_adjacent_public takes no override: powerless, trapped, and receiving nothing from the exercise regime itself, they sit nearest the full-target end of any seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves visible. Reading the regime as pure coordination would hide the externalization — the spread between drilled and demanded competence is carried by people who did not book the savings. Reading it as pure extraction would erase the genuine function: catastrophes cannot be rehearsed live, any regime must use proxies, and simulation does build real procedural and team competence. Mandatrophy is not resolved: the founding problem (safe rehearsal of rare-event competence) is live, so this is not a piton performing a dead mandate; it has no sunset clause, so it is not a scaffold; and it is constructed and actively enforced, not natural, so no mountain claim is available. The open mandatrophy question is directional: whether this reading hardens toward pure extraction as theater grows, or relaxes toward pure coordination as exercise depth catches up with event demands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexical_status,
    'This constraint is one reading of the competence_exercise_validity kernel — would instantiating a sibling reading (real_catastrophe_only or continuous_refresh_hybrid) change the beneficiary/victim structure and epsilon of the arrangement?',
    'Comparative institutional analysis of organizations operating under each reading: exercise budgets, drilled-versus-demanded transfer outcomes, and who bears residual risk under each regime.',
    'Under real_catastrophe_only the training-simulation industry loses its beneficiary position and compliance legitimacy collapses; under continuous_refresh_hybrid the cost profile shifts toward chronic exercise-budget burden rather than latent-risk externalization. The tangled_rope classification authored here holds only for the simulation_as_proxy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexical_status, conceptual, 'Kernel-reading indexicality: classification is relative to the simulation_as_proxy reading of the competence_exercise_validity kernel.').

omega_variable(
    transfer_fidelity_question,
    'What fraction of real-event competence demands are actually exercised by compliance-grade simulation scenarios?',
    'Longitudinal correlation of drill scores with performance in subsequent real incidents; controlled no-notice exercise trials using scenario families outside the trained set.',
    'High transfer fidelity pushes the arrangement toward pure coordination (measured extraction approximates coordination cost); low fidelity pushes toward pure extraction (validation theater collecting legitimacy while readiness decays unseen).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_fidelity_question, empirical, 'The empirical core of the proxy-validity claim: how much of real-event demand the proxy actually rehearses.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression primarily structural (budget lock-in, scheduling power, regulatory acceptance of documentation) or internalized (professional identity fused with certification earned through the exercise regime)?',
    'Post-exit trajectory of critics: operators who leave the licensed track and subsequently dispute drill adequacy reveal how much objection was held down by identity rather than by structure.',
    'If substantially internalized, suppression persists even where structural locks open — reform would require identity-level change in professional culture, not merely regulatory revision of exercise requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism split for the frontline seat.').

omega_variable(
    safety_record_latency_ambiguity,
    'Does the clean safety record prove competence adequacy (this reading''s central warrant) or merely reflect the latency period before competence decay becomes visible in outcomes?',
    'Leading-indicator studies: near-miss analysis, no-notice exercise performance decay curves, and cross-organization comparison of safety records against true exercise depth.',
    'If latency, the reading''s warrant inverts into its strongest refutation — the record measures the depth of the luck pool, not readiness — and the arrangement''s legitimacy claim fails on its own evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_record_latency_ambiguity, empirical, 'Whether the safety record evidences adequacy or masks decay during the latency window.').

omega_variable(
    theater_goodhart_vs_inherent_limit,
    'Does the rising theater_ratio reflect Goodhart drift (compliance metrics substituting for function as the audit ratchet tightens) or an inherent ceiling on how much stress a scheduled, announced proxy can carry?',
    'Natural experiment: organizations adopting unannounced or cross-boundary exercises — if their performative share falls while cost stays bounded, drift rather than inherent limit drives the ratio.',
    'If drift, the arrangement is correctable within the simulation paradigm (harder scenarios, surprise timing, adversarial evaluation); if an inherent limit, the reading itself is structurally capped and the case for deeper exercise formats strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_goodhart_vs_inherent_limit, empirical, 'Source of the performative share in drill activity: metric substitution versus proxy ceiling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 1979, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cev_sim_proxy_tr_t1979, competence_exercise_validity__simulation_as_proxy, theater_ratio, 1979, 0.25).
narrative_ontology:measurement_basis(cev_sim_proxy_tr_t1979, observed).
narrative_ontology:measurement(cev_sim_proxy_tr_t1986, competence_exercise_validity__simulation_as_proxy, theater_ratio, 1986, 0.3).
narrative_ontology:measurement_basis(cev_sim_proxy_tr_t1986, observed).
narrative_ontology:measurement(cev_sim_proxy_tr_t1994, competence_exercise_validity__simulation_as_proxy, theater_ratio, 1994, 0.35).
narrative_ontology:measurement_basis(cev_sim_proxy_tr_t1994, observed).
narrative_ontology:measurement(cev_sim_proxy_tr_t2002, competence_exercise_validity__simulation_as_proxy, theater_ratio, 2002, 0.4).
narrative_ontology:measurement_basis(cev_sim_proxy_tr_t2002, observed).
narrative_ontology:measurement(cev_sim_proxy_tr_t2011, competence_exercise_validity__simulation_as_proxy, theater_ratio, 2011, 0.42).
narrative_ontology:measurement_basis(cev_sim_proxy_tr_t2011, observed).
narrative_ontology:measurement(cev_sim_proxy_tr_t2017, competence_exercise_validity__simulation_as_proxy, theater_ratio, 2017, 0.45).
narrative_ontology:measurement_basis(cev_sim_proxy_tr_t2017, observed).
narrative_ontology:measurement(cev_sim_proxy_tr_t2024, competence_exercise_validity__simulation_as_proxy, theater_ratio, 2024, 0.46).
narrative_ontology:measurement_basis(cev_sim_proxy_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(cev_sim_proxy_be_t1979, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 1979, 0.38).
narrative_ontology:measurement_basis(cev_sim_proxy_be_t1979, observed).
narrative_ontology:measurement(cev_sim_proxy_be_t1986, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 1986, 0.43).
narrative_ontology:measurement_basis(cev_sim_proxy_be_t1986, observed).
narrative_ontology:measurement(cev_sim_proxy_be_t1994, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 1994, 0.48).
narrative_ontology:measurement_basis(cev_sim_proxy_be_t1994, observed).
narrative_ontology:measurement(cev_sim_proxy_be_t2002, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 2002, 0.52).
narrative_ontology:measurement_basis(cev_sim_proxy_be_t2002, observed).
narrative_ontology:measurement(cev_sim_proxy_be_t2011, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 2011, 0.56).
narrative_ontology:measurement_basis(cev_sim_proxy_be_t2011, observed).
narrative_ontology:measurement(cev_sim_proxy_be_t2017, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 2017, 0.59).
narrative_ontology:measurement_basis(cev_sim_proxy_be_t2017, observed).
narrative_ontology:measurement(cev_sim_proxy_be_t2024, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(cev_sim_proxy_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(cev_sim_proxy_su_t1979, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 1979, 0.35).
narrative_ontology:measurement_basis(cev_sim_proxy_su_t1979, observed).
narrative_ontology:measurement(cev_sim_proxy_su_t1986, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement_basis(cev_sim_proxy_su_t1986, observed).
narrative_ontology:measurement(cev_sim_proxy_su_t1994, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 1994, 0.44).
narrative_ontology:measurement_basis(cev_sim_proxy_su_t1994, observed).
narrative_ontology:measurement(cev_sim_proxy_su_t2002, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 2002, 0.47).
narrative_ontology:measurement_basis(cev_sim_proxy_su_t2002, observed).
narrative_ontology:measurement(cev_sim_proxy_su_t2011, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 2011, 0.5).
narrative_ontology:measurement_basis(cev_sim_proxy_su_t2011, observed).
narrative_ontology:measurement(cev_sim_proxy_su_t2017, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 2017, 0.51).
narrative_ontology:measurement_basis(cev_sim_proxy_su_t2017, observed).
narrative_ontology:measurement(cev_sim_proxy_su_t2024, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(cev_sim_proxy_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% The colloquial label 'competence exercise validity' decomposes into three structurally distinct readings of one kernel, each with its own epsilon, beneficiary/victim structure, and classification. This story authors the simulation_as_proxy reading (epsilon 0.62, tangled_rope claim). The sibling files author real_catastrophe_only and continuous_refresh_hybrid; mutual links via affects_constraints form the constraint family. The simulation_as_proxy reading is the institutionally dominant one and shapes the resource environment in which the sibling readings would have to operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, institutional, 0.32).
constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
