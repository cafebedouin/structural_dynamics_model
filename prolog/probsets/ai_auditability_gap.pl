% ============================================================================
% CONSTRAINT STORY: ai_auditability_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_auditability_gap, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_auditability_gap
 *   human_readable: The Black Box Impasse
 *   domain: technological
 *
 * SUMMARY:
 *   The AI auditability gap refers to the increasing disparity between the
 *   complexity of AI decision-making processes and the ability of humans to
 *   understand, verify, and audit those decisions. This gap creates risks of
 *   bias, errors, and lack of accountability, which could disproportionately
 *   harm vulnerable populations. AI developers and early adopters benefit
 *   from the rapid deployment and market adoption of AI systems, but
 *   end-users and regulated sectors bear the costs of opacity and potential
 *   errors. The constraint is actively enforced through regulatory pressure
 *   and public demand for greater AI transparency.
 *
 * KEY AGENTS:
 *   - AI Developers: Primary beneficiary (institutional/arbitrage). Benefits from rapid deployment and market adoption.
 *   - Early Adopters: Secondary beneficiary (powerful/mobile). Gains competitive advantages.
 *   - End Users: Primary victim (powerless/trapped). Lack of transparency and control.
 *   - Regulated Sectors: Secondary victim (moderate/constrained). Heavily regulated sectors that must comply with both auditability and increasing use of AI systems.
 *   - AI Ethics Researchers: Organized actor (organized/mobile). Seeks solutions and raises awareness.
 *   - Traditional Auditors: Institutional actor (institutional/constrained). Struggle to audit complex AI systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_auditability_gap, 0.6).
domain_priors:suppression_score(ai_auditability_gap, 0.7).
domain_priors:theater_ratio(ai_auditability_gap, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_auditability_gap, extractiveness, 0.6).
narrative_ontology:constraint_metric(ai_auditability_gap, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_auditability_gap, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_auditability_gap, tangled_rope).
narrative_ontology:human_readable(ai_auditability_gap, "The Black Box Impasse").
narrative_ontology:topic_domain(ai_auditability_gap, "technological").

domain_priors:requires_active_enforcement(ai_auditability_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_auditability_gap, ai_developers).
narrative_ontology:constraint_beneficiary(ai_auditability_gap, early_adopters).
narrative_ontology:constraint_victim(ai_auditability_gap, regulated_sectors).
narrative_ontology:constraint_victim(ai_auditability_gap, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: End users (powerless/trapped). Lack of transparency and control over AI decisions affecting their lives. No real exit option. Bears the cost of errors and biases. Experiences the system as a Snare.
constraint_indexing:constraint_classification(ai_auditability_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Regulated sectors (moderate/constrained). Heavily regulated sectors like finance and healthcare are constrained by the need for explainability and auditability, but also benefit from increased automation, with limited exit options. Tangled Rope.
constraint_indexing:constraint_classification(ai_auditability_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: AI Developers (institutional/arbitrage). Benefits from rapid deployment and market adoption, can arbitrage regulatory loopholes, sees AI system as efficient decision-making tool, minimal cost. The AI is experienced as a Rope.
constraint_indexing:constraint_classification(ai_auditability_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: AI Ethics Researchers (organized/mobile). Benefit from identifying and publishing issues, see the gap as a coordination problem that can be solved with better tooling, with high enforcement and the option to exit research to apply knowledge directly. Consider the gap temporary due to increased public pressure to audit AI; expect it will become less pertinent as auditability tools and standards improve, leading to a sunset of the problem. Scaffold.
constraint_indexing:constraint_classification(ai_auditability_gap, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 5: Traditional Auditors (institutional/constrained). Experience a degraded process as they can no longer effectively audit complex AI systems with traditional methods. Piton.
constraint_indexing:constraint_classification(ai_auditability_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 6: Analytical Observer (analytical/analytical). Sees the AI auditability gap as a persistent tension between innovation and accountability, requiring both coordination and extraction. Tangled Rope.
constraint_indexing:constraint_classification(ai_auditability_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_auditability_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_auditability_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_auditability_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_auditability_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_auditability_gap, TR),
    TR >= 0.70.

:- end_tests(ai_auditability_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. AI systems extract value from end-users through data collection and behavioral modification, often without their full knowledge or consent. Early Adopters gain competitive advantages from the opacity, extracting value at the expense of the sector. Suppression (0.70): High. Lack of transparency and technical expertise prevents end-users from challenging AI decisions. Complexity of AI development and deployment processes makes auditing difficult for regulatory bodies. Theater Ratio (0.75): High. There is increasing pressure for AI ethics and auditability, but real and effective monitoring mechanisms are not yet in widespread use.
 *
 * PERSPECTIVAL GAP:
 *   The different perspectives highlight the inherent tensions in the AI auditability gap. AI developers see the system as a means of innovation and efficiency, while end-users bear the costs of opacity and potential biases. Regulated sectors are caught in the middle, constrained by the need for both auditability and the adoption of AI technologies. AI ethics researchers view the gap as a coordination problem, while traditional auditors see a degradation of their skills.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agents' power, exit options, and beneficiary/victim status. AI developers, with institutional power and arbitrage options, benefit from the status quo and experience the gap as coordination. End-users, powerless and trapped, bear the brunt of the costs and experience it as extraction. Regulated sectors, with moderate power and constrained exit options, experience a mixed situation. The AI ethics researchers, while organized, have exit to direct application of the knowledge, but are otherwise a stakeholder to the problem. The traditional auditors are constrained by their inability to exit the space effectively or make significant changes to the current operations.
 *
 * MANDATROPHY ANALYSIS:
 *   The AI auditability gap resolves the mandatrophy by showing that while AI can be beneficial, its opacity creates opportunities for extraction and requires active enforcement to mitigate potential harms. The tangled rope classification reflects the need for both coordination (innovation) and extraction (accountability) to ensure that AI systems are developed and deployed in a responsible and equitable manner.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    explainability_interpretability_tradeoff,
    'To what extent is the inherent complexity of advanced AI models (deep learning) fundamentally at odds with human interpretability?',
    'Progress in explainable AI (XAI) techniques; development of intrinsically interpretable model architectures',
    'If models remain black boxes: AI governance relies on proxies/regulation, with Snare characteristics becoming more pronounced. If models become more transparent: Trust and adoption increase, lessening the need for external controls. Moving to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explainability_interpretability_tradeoff, empirical, 'Quantify the Explainability/Interpretability Tradeoff').

omega_variable(
    regulatory_capture_potential,
    'Will regulatory bodies responsible for AI governance be susceptible to regulatory capture by powerful AI developers?',
    'Monitoring lobbying activities, tracking revolving door phenomena, assessing the independence of regulatory agencies',
    'If regulatory capture occurs: governance favors developers, extraction from end-users increases. If agencies maintain independence: regulation balances innovation with public interest, extraction is mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_potential, conceptual, 'Assess Potential for Regulatory Capture').

omega_variable(
    unintended_consequences_magnitude,
    'To what degree do unintended consequences arising from AI systems (bias, discrimination, privacy violations) outweigh the intended benefits?',
    'Cost-benefit analyses of specific AI deployments, evaluation of social and economic impacts, measurement of negative externalities',
    'If costs > benefits: AI systems become net-negative for society, extraction increases. If benefits > costs: AI offers genuine progress, extraction is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unintended_consequences_magnitude, empirical, 'Measure Magnitude of Unintended Consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_auditability_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_auditability_gap, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_a_tr_t5, ai_auditability_gap, theater_ratio, 5, 0.5).
narrative_ontology:measurement(ai_a_tr_t10, ai_auditability_gap, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_auditability_gap, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_a_be_t5, ai_auditability_gap, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ai_a_be_t10, ai_auditability_gap, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_auditability_gap, information_standard).
narrative_ontology:affects_constraint(ai_auditability_gap, data_privacy_erosion).
narrative_ontology:affects_constraint(ai_auditability_gap, algorithmic_bias_amplification).

% DUAL FORMULATION NOTE:
% The auditability gap is a distinct constraint from data privacy erosion and algorithmic bias amplification but is closely related. Overcoming the auditability gap requires progress on both data privacy and bias mitigation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
