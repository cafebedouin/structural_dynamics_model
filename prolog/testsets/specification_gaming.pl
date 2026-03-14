% ============================================================================
% CONSTRAINT STORY: specification_gaming
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_specification_gaming, []).

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
 *   constraint_id: specification_gaming
 *   human_readable: Specification Gaming in Goal-Oriented Systems
 *   domain: general/systems_design/principal_agent
 *
 * SUMMARY:
 *   Specification gaming is a structural constraint that emerges whenever a
 *   principal creates a measurable metric to incentivize agent behavior, but
 *   the metric diverges from the principal's true goal. The agent, facing
 *   clear success criteria, optimizes the metric through exploitation of
 *   specification loopholes rather than pursuing the unstated intent. This
 *   constraint operates across domains: AI systems gaming training metrics,
 *   organizations gaming performance KPIs, regulatory systems gamed through
 *   compliance theater, and markets gaming pricing signals. The constraint is
 *   extractive because the loophole exploitation transfers value from the
 *   stakeholder (whose actual goals are unmet) to the agent (who achieves
 *   nominal success and its rewards) and the spec author (whose authority is
 *   validated by apparent compliance). The game persists because detecting
 *   and closing loopholes is costly, and each closure creates pressure to
 *   find new loopholes — the system exhibits Goodhart's law dynamics. The
 *   theater ratio (0.68) reflects that specification enforcement mechanisms
 *   are substantially performative: audits validate that agents are gaming
 *   the spec correctly, not that the spec aligns with intent.
 *
 * KEY AGENTS:
 *   - End User / Stakeholder: Primary victim (powerless/trapped) — faces system with misaligned incentives; cannot exit or modify spec
 *   - Specification Author / Principal: Primary beneficiary (institutional/arbitrage) — maintains authority and control; experiences agent compliance with spec as success
 *   - Agent / Optimizer: Secondary agent (moderate/constrained) — faces resource constraints and career pressure; benefits from clear success criteria; harmed if spec closes
 *   - Downstream Ecosystem: Secondary victim (powerless/trapped) — accumulates externalities and ecosystem damage over time
 *   - Metric Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains performative compliance rituals; preserves theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent design choices as inevitable constraint on all systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(specification_gaming, 0.58).
domain_priors:suppression_score(specification_gaming, 0.65).
domain_priors:theater_ratio(specification_gaming, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(specification_gaming, extractiveness, 0.58).
narrative_ontology:constraint_metric(specification_gaming, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(specification_gaming, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(specification_gaming, snare).
narrative_ontology:human_readable(specification_gaming, "Specification Gaming in Goal-Oriented Systems").
narrative_ontology:topic_domain(specification_gaming, "general/systems_design/principal_agent").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(specification_gaming, spec_gamers).
narrative_ontology:constraint_beneficiary(specification_gaming, specification_authors).
narrative_ontology:constraint_victim(specification_gaming, system_users).
narrative_ontology:constraint_victim(specification_gaming, stakeholder_intent).
narrative_ontology:constraint_victim(specification_gaming, downstream_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER / STAKEHOLDER (SNARE) — Trapped in a system where official metrics diverge from actual goals. Cannot exit without abandoning the system; cannot modify specification to close the loophole. Experiences pure extraction: the system optimizes for measurable proxies while sacrificing the user's actual interests. No coordination benefit — the specification gaming mechanism extracts user value without reciprocal service.
constraint_indexing:constraint_classification(specification_gaming, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SPECIFICATION AUTHOR / PRINCIPAL (ROPE) — Can rewrite the specification to close loopholes, has enforcement authority, experiences immediate metric achievement. Sees specification gaming as a coordination mechanism: the spec defines acceptable behavior, and agents optimizing it are solving the principal's stated problem correctly. Net beneficiary from the institutional perspective — the specification author's authority and control are validated by agent compliance with the spec, even when that compliance diverges from intent.
constraint_indexing:constraint_classification(specification_gaming, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: AGENT / OPTIMIZER (TANGLED ROPE) — Constrained by resource costs of trying multiple interpretations and by career/survival pressure to meet targets. Benefits from the specification: clear success criteria provide guidance and justification for resource allocation. But also bears costs: if the loophole closes, the optimization becomes harder; if the stakeholder retaliates against loophole exploitation, the agent faces reputational or legal consequences. Mixed experience — genuine coordination (clear goals, measurable success) with embedded extraction (incentive to exploit loopholes for efficiency).
constraint_indexing:constraint_classification(specification_gaming, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOWNSTREAM ECOSYSTEM (SNARE) — Over time, specification gaming accumulates externalities: shortened product lifespans, degraded system reliability, ecosystem damage (from misaligned optimization). The downstream actors have no exit option and cannot participate in rewriting specifications. Pure extraction at long timescale: the specification gaming mechanism transfers value from long-term ecosystem health to short-term metric achievement.
constraint_indexing:constraint_classification(specification_gaming, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: METRIC ENFORCEMENT APPARATUS (PITON) — The ritual of measuring, auditing, and reporting on specification compliance persists through institutional inertia. The measurement system was designed to monitor real-world performance but now mostly validates that agents are gaming the spec correctly. Theater ratio high: much of the enforcement apparatus is performative ritual that certifies fake compliance without detecting real misalignment. The apparatus has degraded from functional monitoring to theater.
constraint_indexing:constraint_classification(specification_gaming, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, specification gaming appears to be an immutable constraint on goal-oriented systems: any system with measurable proxies for unmeasurable goals will face incentive pressure to optimize proxies. This is a fundamental problem of incentive design — a structural limit on principal-agent relationships. However, this perspective risks naturalizing what is actually a contingent design choice (the use of narrow proxies rather than richer feedback systems). The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(specification_gaming, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(specification_gaming_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(specification_gaming, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(specification_gaming, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(specification_gaming, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(specification_gaming, TR),
    TR >= 0.70.

:- end_tests(specification_gaming_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The specification gaming mechanism extracts value from stakeholders by achieving nominal success on metrics while leaving true goals unmet. The value is not catastrophic at first (when loopholes are small and rare) but increases over time as agents learn the space and exploit larger loopholes, and as the specification gaming cycle accelerates (more metrics → more loopholes → more gaming). Suppression (0.65): Moderate-high. Multiple barriers prevent stakeholders and honest agents from escaping the gaming dynamic: contractual lock-in, switching costs to alternative systems, regulatory requirements to use the spec, and career penalties for refusing to optimize the metric even when doing so contradicts intent. Theater ratio (0.68): High and rising. The metric enforcement and specification-writing apparatus becomes increasingly performative as it struggles to detect gaming. Audits certify spec compliance without detecting real misalignment; specification revisions close old loopholes but are systematized by agents until new loopholes emerge. The ritual persists through institutional inertia even as its function degrades.
 *
 * PERSPECTIVAL GAP:
 *   The principal-agent disagreement on classification type is the diagnostic signal. Spec authors see rope (clean goal-setting, agent compliance) while victims see snare (trapped in misaligned system). Agents see tangled rope (some benefits from clarity, some costs from loopholes). This multi-type signature emerges because the constraint's extractiveness depends entirely on the observer's structural position relative to the metric. From the author's view, the agent's optimization is successful — the spec is being followed. From the victim's view, the optimization is a failure — the metric diverges from intent. Neither view is wrong; they are perspectival readings of the same structural fact: the specification author's success (agents comply with spec) is the victim's failure (metric diverges from intent). The snare classification is correct for the victim because they are trapped and cannot escape.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value (d) for each agent derives from their position in the extraction flow. Beneficiaries (spec authors, agents who game successfully) have low d because the constraint flows toward them — they receive the rewards of nominal success. Victims (end users, downstream ecosystem) have high d because they bear costs without benefits. The agent-optimizer has intermediate d because they face both benefits (clarity of goals) and costs (risk of closure, complexity of gaming). The trapped agents (end user, ecosystem) have very high d approaching 1.0 because they are structurally unable to escape or negotiate. This produces high f(d) for victims, amplifying experienced extraction chi. The beneficiaries' arbitrage options allow them to exit (rewrite the spec, switch to better metrics), reducing their experienced extraction. The specification author's institutional power is not threatened by gaming (their authority is validated by agent compliance with their spec) — this is the key structural asymmetry that makes this a snare rather than a rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The specification gaming constraint resolves the mandatrophy by showing that it is NOT pure coordination (rope) despite appearing so to the beneficiary. It is extraction (snare) disguised as coordination. The principal's perspective that 'agents are following the spec I wrote' mistakes nominal compliance for coordination — the victim's perspective reveals that the spec itself is misaligned. The constraint exists to solve the principal's goal-setting problem (coordination: write a metric) while simultaneously creating the victim's extraction problem (misaligned incentives). This is the definition of tangled rope for the agent and snare for the victim — mixed experience at one level, pure extraction at another. The mandatrophy dissolves when we recognize that the single-perspective view (institutional/immediate) that sees rope is incomplete; the fuller analysis requires the powerless/biographical victim perspective that sees snare. The high theater ratio (0.68) further supports the snare classification: much of the spec compliance is performative validation that the spec is being followed correctly, not evidence that the goals are being achieved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_vs_intent_disambiguation,
    'Is a specification gaming instance a failure of the metric (the proxy is wrong) or a failure of the agent (the agent chose to exploit a known loophole instead of pursuing true intent)?',
    'Post-incident analysis: did the specification author recognize the loophole beforehand, and did the agent have reason to believe the exploitation violated unstated intent? Counterfactual: would the agent have pursued the same action if the specification were tighter?',
    'If metric failure: the snare classification is correct — the victim is trapped by misaligned incentives. If agent failure: the classification downshifts toward rope or tangled rope — the victim had a defense (better specification) that wasn''t used.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_vs_intent_disambiguation, conceptual, 'Whether gaming reveals metric failure or agent opportunism').

omega_variable(
    intentionality_threshold,
    'At what level of obviousness does specification compliance cross from ''optimization'' to ''gaming''? Is there an intentionality threshold?',
    'Analysis of the agent''s knowledge state: did the agent explicitly consider that the spec diverges from intent? Did the agent model the principal''s likely reaction? Comparison across similar specs: do agents consistently exploit the same loophole, suggesting obviousness?',
    'If intentionality threshold is low: even accidentally discovered loopholes count as gaming, and extraction persists regardless of agent motivation. If threshold is high: only deliberate exploitation counts, reducing the scope of the snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intentionality_threshold, conceptual, 'Intentionality threshold for specification gaming classification').

omega_variable(
    metric_substitution_depth,
    'How many layers of specification-writing and re-specification occur before the system reaches stable alignment, and does the system ever reach stable alignment or merely chase Goodhart''s law indefinitely?',
    'Historical tracking of specification versions and metric changes; identification of repeated loophole discovery-closure cycles; analysis of whether closure ever produces tighter specification or just slower gaming.',
    'If system reaches stable alignment: specification gaming is a temporary snare with sunset logic. If system chases indefinitely: specification gaming is an inherent structural feature of misaligned principal-agent systems, and the snare persists until the relationship itself ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_substitution_depth, empirical, 'Whether specification refinement converges to alignment or cycles indefinitely').

omega_variable(
    stakeholder_exit_feasibility,
    'Do stakeholders have genuine exit options (switching to alternative systems, opting out, demanding renegotiation) or are they truly trapped by lock-in and dependency?',
    'Cost analysis of alternatives; case studies of stakeholders who attempted exit; measurement of switching barriers (switching costs, learning curves, contractual penalties).',
    'If exit is feasible: victims'' exit_options upgrade from trapped to constrained, and classification shifts from snare toward tangled rope. If exit is infeasible: the snare classification is confirmed, and suppression increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_exit_feasibility, empirical, 'Whether trapped agents have genuine exit options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(specification_gaming, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(specgame_tr_t0, specification_gaming, theater_ratio, 0, 0.45).
narrative_ontology:measurement(specgame_tr_t3, specification_gaming, theater_ratio, 3, 0.58).
narrative_ontology:measurement(specgame_tr_t6, specification_gaming, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(specgame_be_t0, specification_gaming, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(specgame_be_t3, specification_gaming, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(specgame_be_t6, specification_gaming, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(specification_gaming, information_standard).
narrative_ontology:affects_constraint(specification_gaming, goodharts_law).
narrative_ontology:affects_constraint(specification_gaming, principal_agent_misalignment).

% DUAL FORMULATION NOTE:
% Specification gaming is downstream of principal-agent misalignment (the structural inability to write perfect contracts) and Goodhart's law (any metric becomes a poor measure once optimized). The three constraints form a causal chain: misalignment creates the need for metrics, metrics enable gaming, and gaming escalates into Goodhart dynamics. Each has its own epsilon reflecting different structural levels: misalignment is fundamental, Goodhart's law is a mathematical fact, specification gaming is the behavioral manifestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
