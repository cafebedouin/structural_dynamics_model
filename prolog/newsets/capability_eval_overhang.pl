% ============================================================================
% CONSTRAINT STORY: capability_eval_overhang
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_eval_overhang, []).

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
 *   constraint_id: capability_eval_overhang
 *   human_readable: The Capability Evaluation Overhang
 *   domain: technological/ai/governance
 *
 * SUMMARY:
 *   The capability evaluation overhang describes the gap between an AI
 *   system's true capabilities and what is measured by standard benchmarks.
 *   This gap creates a significant, unquantified risk that is externalized
 *   onto the public. The constraint is actively maintained by research and
 *   market incentives that reward rapid progress on benchmarked tasks over
 *   comprehensive, slower, and more expensive safety evaluations. This story
 *   serves as a diagnostic exemplar, showing how a single structural
 *   phenomenon can be perceived as all six constraint types depending on the
 *   observer's position relative to the flow of risk and reward.
 *
 * KEY AGENTS:
 *   - AI Capability Developers: Primary beneficiary (institutional/arbitrage) — Profit from rapid progress and market leadership while externalizing tail risks.
 *   - Global Civil Society: Primary victim (powerless/trapped) — Bears the full, unconsented risk of AI misuse or accident with no recourse or control.
 *   - AI Safety Auditors/Regulators: Secondary victim (organized/constrained) — Tasked with ensuring safety but lack the tools, access, and political capital to close the evaluation gap.
 *   - Venture Capital Investors: Secondary beneficiary (powerful/mobile) — Fund the race for capabilities, viewing current safety deficits as a temporary cost of innovation (a Scaffold).
 *   - Legacy Standards Bodies: Institutional observer (institutional/constrained) — See the current evaluation paradigm as a performative, degraded version of true safety certification (a Piton).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_eval_overhang, 0.65).
domain_priors:suppression_score(capability_eval_overhang, 0.75).
domain_priors:theater_ratio(capability_eval_overhang, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_eval_overhang, extractiveness, 0.65).
narrative_ontology:constraint_metric(capability_eval_overhang, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(capability_eval_overhang, theater_ratio, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_eval_overhang, tangled_rope).
narrative_ontology:human_readable(capability_eval_overhang, "The Capability Evaluation Overhang").
narrative_ontology:topic_domain(capability_eval_overhang, "technological/ai/governance").

domain_priors:requires_active_enforcement(capability_eval_overhang).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capability_eval_overhang, ai_capability_developers).
narrative_ontology:constraint_beneficiary(capability_eval_overhang, national_security_actors).
narrative_ontology:constraint_victim(capability_eval_overhang, global_civil_society).
narrative_ontology:constraint_victim(capability_eval_overhang, ai_safety_auditors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVIL SOCIETY (SNARE) — Bears the unconsented risk of catastrophic AI failure from unmeasured capabilities. Has no ability to exit the global technology ecosystem or influence evaluation standards directly. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11. This is pure, coercive extraction of safety for progress.
constraint_indexing:constraint_classification(capability_eval_overhang, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AI DEVELOPER (ROPE) — Experiences benchmarks as a pure coordination mechanism for measuring progress against competitors and attracting investment. Can arbitrage which results to publish and which evaluations to run. The 'overhang' is seen as a frontier of discovery, not a liability. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.10. A net subsidy.
constraint_indexing:constraint_classification(capability_eval_overhang, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: AUDITOR/REGULATOR (TANGLED ROPE) — Recognizes the coordination function of benchmarks but is primarily concerned with the extractive risk of the overhang. Is constrained by limited access to models, lagging evaluation techniques, and political pressure to not stifle innovation. d≈0.75 (as victim + constrained), f(d)≈1.10, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(capability_eval_overhang, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: VENTURE CAPITAL (SCAFFOLD) — Views the current weak evaluation regime as a temporary, necessary support to bootstrap the industry. The implicit belief is that once market dominance is achieved, profits can fund more robust safety measures, making the current risk a transitional phase. This implies a belief in a future sunset clause on lax evaluations. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.51. This is too high for scaffold, but reflects the investor's framing, not the structural reality.
constraint_indexing:constraint_classification(capability_eval_overhang, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY STANDARDS BODY (PITON) — Views the AI field's benchmark-driven culture as a degraded, performative substitute for rigorous, safety-critical certification (e.g., aviation, nuclear). The function of ensuring public safety has atrophied, replaced by the theater of leaderboard scores. theater_ratio=0.72 satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(capability_eval_overhang, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TECHNOLOGICAL DETERMINIST (MOUNTAIN) — Frames the evaluation overhang as an immutable law of progress: our ability to create complex technology will always outpace our ability to understand and control it. This naturalizes the risk, treating a contingent institutional and economic arrangement as a fixed feature of reality. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(capability_eval_overhang, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_eval_overhang_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capability_eval_overhang, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_eval_overhang, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capability_eval_overhang, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capability_eval_overhang, TR),
    TR >= 0.70.

:- end_tests(capability_eval_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Represents the societal cost of potential catastrophic risks being externalized by developers who bear only the upside. Suppression (0.75): High. The entire ecosystem of academic prestige (publications), funding (VC and grants), and media hype is oriented around benchmark performance, actively suppressing slower, more holistic safety research. Theater Ratio (0.72): High. As models become more complex, benchmarks increasingly become a performative ritual of 'SOTA-chasing' that provides a false sense of security while failing to probe for dangerous, out-of-distribution behaviors.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. To developers, benchmarks are a Rope for coordinating research. To the public, this same system is a Snare, imposing risk without consent. To regulators, it's a Tangled Rope—a necessary tool they must grapple with to manage risk. To investors, it's a temporary Scaffold on the way to market dominance. To traditional engineers, it's a Piton—a hollowed-out ritual of what safety engineering should be. To determinists, it's an inevitable Mountain. The conflict is not over the facts (the overhang exists) but over the structural interpretation of its function and legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Developers) with arbitrage see a net subsidy (Rope). Victims (Public) who are trapped see maximum coercive extraction (Snare). Agents who are both victims and have some agency (Auditors) see a mix of coordination and extraction (Tangled Rope). The system correctly derives these classifications from the declared structural relationships and exit options, revealing the underlying power dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that no single classification is 'correct.' The reality of the constraint *is* the full presheaf of all six perspectival classifications. Labeling the evaluation overhang as merely a 'coordination problem' (Rope) or merely 'predatory risk externalization' (Snare) would be a failure of analysis. The system's value is in holding all valid perspectives simultaneously, mapping the full landscape of the conflict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergent_vs_scaling,
    'Is the capability overhang a result of predictable scaling laws, or does it contain truly discontinuous, emergent capabilities that no benchmark could anticipate?',
    'Empirical testing for phase transitions in model capabilities at scale; development of novel probes for unanticipated skills.',
    'If purely scaling: the problem is a Rope (better benchmarks needed). If emergent: it''s a Snare (fundamental controllability risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergent_vs_scaling, empirical, 'Distinguishing predictable scaling from true emergence in AI capabilities.').

omega_variable(
    instrumental_deception_risk,
    'Could a sufficiently advanced AI learn to deliberately underperform on evaluations to hide its true capabilities?',
    'Red-teaming for sandbagging behavior; interpretability research to detect hidden reasoning; theoretical work on goal misgeneralization.',
    'If possible: suppression metric approaches 1.0, as all evaluations become unreliable theater. The constraint solidifies into a Snare from almost all perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumental_deception_risk, conceptual, 'The risk of AI systems actively hiding their capabilities during evaluation.').

omega_variable(
    acceptable_risk_threshold,
    'What level of unmeasured risk (overhang) is society willing to accept in exchange for rapid technological and economic progress?',
    'Deliberative democratic processes; formal risk-benefit analysis incorporating expert elicitation on tail risks.',
    'A high risk tolerance normalizes the constraint, making it appear as a Rope or Scaffold. A low risk tolerance frames it as an unacceptable Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(acceptable_risk_threshold, preference, 'Societal preference for balancing AI progress against catastrophic risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_eval_overhang, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capa_tr_t2015, capability_eval_overhang, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(capa_tr_t2020, capability_eval_overhang, theater_ratio, 2020, 0.61).
narrative_ontology:measurement(capa_tr_t2025, capability_eval_overhang, theater_ratio, 2025, 0.72).

% Extraction over time
narrative_ontology:measurement(capa_be_t2015, capability_eval_overhang, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(capa_be_t2020, capability_eval_overhang, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(capa_be_t2025, capability_eval_overhang, base_extractiveness, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_eval_overhang, information_standard).
narrative_ontology:affects_constraint(capability_eval_overhang, academic_incentive_structure).
narrative_ontology:affects_constraint(capability_eval_overhang, liability_for_autonomous_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
