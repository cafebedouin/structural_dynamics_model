% ============================================================================
% CONSTRAINT STORY: memetic_fitness_vs_truth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_memetic_fitness_vs_truth, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: memetic_fitness_vs_truth
 *   human_readable: The Viral Distortion: Memetic Fitness vs. Truth
 *   domain: social/informational/technological
 *
 * SUMMARY:
 *   The viral distortion constraint describes the structural decoupling
 *   between how successfully an idea spreads (memetic fitness) and whether it
 *   accurately maps to reality (truth). In pre-digital information
 *   environments, these were weakly decoupled — word-of-mouth transmission
 *   had modest amplification, and false claims faced natural friction from
 *   experienced reality and direct social correction. In the
 *   digital-algorithmic age, the decoupling has become severe:
 *   engagement-optimized ranking systems (rewarding emotional intensity,
 *   novelty, polarization) actively amplify content inversely proportional to
 *   epistemic reliability. The constraint exhibits the classic Tangled Rope
 *   structure: platforms provide genuine coordination (connecting creators to
 *   audiences efficiently) while simultaneously extracting from the epistemic
 *   commons (degrading shared information quality). This extraction is
 *   enforced through algorithmic amplification mechanisms that suppress
 *   corrective content and alternative ranking schemas, and is maintained
 *   through institutional inertia (platforms benefit from engagement metrics
 *   and have little incentive to change). The theater_ratio of 0.81 reflects
 *   that formal fact-checking and content moderation have become largely
 *   performative — reactions to viral claims rather than preventing their
 *   spread, compliance theater rather than epistemic protection.
 *
 * KEY AGENTS:
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good bearing full cost of distortion; cannot exit or organize
 *   - Truth-Tracking Communities: Secondary victim (moderate/trapped) — scientists, journalists, fact-checkers trapped by obligation to correct false claims spreading faster than corrections
 *   - Content Creators (Adaptive): Dual role (powerful/mobile) — benefit from algorithmic amplification but also exploit false/exaggerated framing for virality; can exit strategically
 *   - Algorithmic Amplification Operators: Primary beneficiary (institutional/arbitrage) — platforms benefit from engagement-optimized ranking with exit option (could change algorithms) that is not exercised
 *   - Attention Capture Entities: Secondary beneficiary (institutional/arbitrage) — commercial actors optimizing for virality; see constraint as pure coordination
 *   - Institutional Fact-Checking / Moderation: Degraded institutional actor (institutional/constrained) — maintains theatrical verification ritual despite low functional efficacy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent algorithmic architecture as inevitable information dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(memetic_fitness_vs_truth, 0.58).
domain_priors:suppression_score(memetic_fitness_vs_truth, 0.68).
domain_priors:theater_ratio(memetic_fitness_vs_truth, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, extractiveness, 0.58).
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(memetic_fitness_vs_truth, tangled_rope).
narrative_ontology:human_readable(memetic_fitness_vs_truth, "The Viral Distortion: Memetic Fitness vs. Truth").
narrative_ontology:topic_domain(memetic_fitness_vs_truth, "social/informational/technological").

domain_priors:requires_active_enforcement(memetic_fitness_vs_truth).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(memetic_fitness_vs_truth, attention_capture_entities).
narrative_ontology:constraint_beneficiary(memetic_fitness_vs_truth, algorithmic_amplification_operators).
narrative_ontology:constraint_victim(memetic_fitness_vs_truth, epistemic_commons).
narrative_ontology:constraint_victim(memetic_fitness_vs_truth, truth_tracking_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The shared informational environment cannot exit the constraint; bears full cost of distortion. No exit mechanism exists for correcting mass-distributed false claims. d≈0.96, f(d)≈1.42, σ=1.2 → χ≈0.98. Maximum extraction.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRUTH-TRACKING COMMUNITIES (SNARE) — Fact-checkers, scientists, journalists cannot exit; trapped by epistemic obligation to counter false claims that spread faster than corrections. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.95.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATORS / ADAPTIVE AGENTS (TANGLED ROPE) — Benefit from algorithmic amplification (coordination: reaching audiences efficiently). Simultaneously exploit false/exaggerated framing for virality (extraction from epistemic commons). Can strategically exit by shifting to lower-engagement content. d≈0.58, f(d)≈0.88, σ=1.2 → χ≈0.61. Mixed coordination and extraction.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ALGORITHMIC AMPLIFICATION OPERATORS / PLATFORMS (ROPE) — Benefit from engagement-optimized ranking (institutional arbitrage advantage). See constraint as coordination function: connecting audiences to content at scale. Have exit option (could change ranking algorithms) but do not exercise it. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ATTENTION CAPTURE ENTITIES / COMMERCIAL ACTORS (ROPE) — Benefit from virality-optimized content strategies; see the constraint as pure coordination problem: reaching attention-seekers efficiently. d≈0.12, f(d)≈-0.05, σ=1.2 → χ≈-0.03. Net beneficiary perspective.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL FACT-CHECKING / CONTENT MODERATION (PITON) — Formal fact-checking and moderation systems have atrophied as real verification mechanisms; now largely theatrical compliance and post-hoc reaction. theater_ratio=0.81 satisfies piton gate. d≈0.52, f(d)≈0.77, σ=1.2 → χ≈0.61. Degraded constraint maintained by institutional inertia despite low functional verification.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN?) — From a civilizational perspective, the decoupling of memetic fitness from truth could be seen as inherent to biological/cultural evolution: fitness is always a proxy for survival, not accuracy. However, base properties (ε=0.58, suppression=0.68, theater=0.81) contradict mountain classification — this is a false summit. The constraint is contingent on digital-age amplification architecture, not inevitable.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(memetic_fitness_vs_truth_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(memetic_fitness_vs_truth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(memetic_fitness_vs_truth, TR),
    TR >= 0.70.

:- end_tests(memetic_fitness_vs_truth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from truth-tracking capacity; false claims propagate faster than corrections and gain disproportionate attention. However, extraction is not maximal (0.68+) because some false claims eventually face consequences, alternative platforms exist at the margin, and some users actively seek truth-tracking sources. The measurement trajectory (0.22 → 0.42 → 0.58) shows extraction increasing over the interval as algorithmic systems have become more sophisticated at engagement optimization. Suppression (0.68): High. Barriers to counter-narrative include: (a) algorithmic ranking actively suppresses corrective content; (b) psychological susceptibility to first-exposure effect and emotional resonance; (c) institutional barriers to rapid truth-production (peer review, journalism timelines); (d) economic incentives favoring engagement over accuracy; (e) cognitive load preventing deep verification. These are substantial but not absolute — exit mechanisms exist for motivated actors. Theater ratio (0.81): Very high. Formal fact-checking and content moderation have become substantially performative: post-hoc reactions to viral claims, compliance theater for regulatory pressure, selective enforcement. The actual epistemic work (distributed community scrutiny, experimental verification, investigative reporting) is decoupled from the institutional moderation infrastructure. Theater ratio has increased over the interval (0.35 → 0.81) as the scale of viral claims has exceeded institutional fact-checking capacity, leaving moderation as a performative gesture rather than functional gate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival gap. Platforms and attention-capture entities see pure coordination (Rope) — they solve the genuine problem of scale-matching: connecting creators to relevant audiences. Truth-tracking communities and the epistemic commons see pure extraction (Snare) — they bear the cost with no exit. Adaptive content creators see a mixed scenario (Tangled Rope) — they benefit from algorithmic reach while also exploiting the fitness-truth gap for amplification advantage. Institutional fact-checking sees its own degradation (Piton) — moderation persists as a ritual despite reduced function. The analytical observer risks naturalizing the constraint as inherent information dynamics (Mountain) — a false summit because the decoupling is contingent on specific architectural choices (engagement-optimized ranking), not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Epistemic Commons: Victim + trapped → d≈0.96, f(d)≈1.42. Maximum extraction. Truth-Tracking Communities: Victim + trapped (organized response capacity) → d≈0.92, f(d)≈1.38. High extraction despite institutional organization; caught between epistemic obligation and inability to match viral timescales. Adaptive Content Creators: Beneficiary + mobile → d≈0.58, f(d)≈0.88. Mixed directionality; benefit from amplification but also have exit option (can reduce sensationalism). Platforms: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Strong net beneficiary; have direct control over ranking algorithms. Attention-Capture Entities: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Net beneficiary. Fact-Checking/Moderation: Victim + constrained → d≈0.52, f(d)≈0.77. Moderate extraction; institutional actors but constrained by scale and engagement economics.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE MANDATROPHY: Is this constraint a coordination problem (platforms efficiently connecting creators to audiences) or an extraction mechanism (platforms amplifying false claims to damage epistemic commons for engagement profit)? The tangled rope classification resolves this by asserting BOTH are structurally true. Platforms do solve a coordination problem (there is genuine value in large-scale content distribution). Simultaneously, they extract from epistemic quality to optimize for engagement metrics. The key insight: platforms have an exit option (change ranking algorithms to deprioritize engagement) that they do not exercise, because engagement maximization is their core business model. The mandatrophy is resolved by recognizing that the 'coordination' framing (what platforms emphasize) and the 'extraction' framing (what truth-trackers experience) are both accurate descriptions of the same structural phenomenon. The constraint is NOT a pure rope (then platforms would have incentive to reduce engagement optimization). It IS a tangled rope because coordination benefit coexists with asymmetric extraction, and asymmetric enforcement (algorithmic ranking actively suppresses alternatives) maintains the imbalance. Theater_ratio (0.81) confirms institutional inertia: formal moderation maintains the appearance of verification control while actual epistemic work happens elsewhere (community scrutiny, peer review, independent journalism). The constraint would degrade from Tangled Rope to Piton if platforms explicitly abandoned any pretense of balancing accuracy with engagement — instead they maintain both the coordination function AND the performative moderation ritual, satisfying the tangled rope requirement of ACTIVE ENFORCEMENT.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amplification_feedback_reversal,
    'Can algorithmic ranking systems be fundamentally reversed to prioritize truth-tracking signals (citation verification, source credibility, peer review status) without collapsing user engagement and platform viability?',
    'Experimental redesign of ranking algorithms; measurement of engagement metrics and epistemic quality under alternative fitness functions; economic viability analysis of truth-optimized platforms',
    'If reversible: constraint becomes purely coordination problem (Rope across perspectives). If not reversible: constraint is structural (economic/psychological irreversibility), and current classification as Tangled Rope is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amplification_feedback_reversal, empirical, 'Whether amplification feedback can be reversed toward truth').

omega_variable(
    cognitive_availability_trap,
    'Is the human cognitive substrate inherently susceptible to emotional/sensational content, or is this susceptibility being amplified by systematic viral reinforcement?',
    'Cross-cultural studies of information diffusion in communities with and without algorithmic amplification; historical analysis of rumor spread pre-internet; neurological studies of attention capture mechanisms',
    'If inherent to cognition: suppression floor (0.68) is a natural law. If amplified by architecture: suppression is lower in less-amplified contexts, suggesting structural rather than fundamental constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_availability_trap, empirical, 'Whether viral susceptibility is cognitive or amplification-driven').

omega_variable(
    truth_production_lag,
    'What is the minimum timeline required for truth-tracking institutions (peer review, science, journalism) to verify and communicate accurate alternatives to viral false claims?',
    'Longitudinal measurement of claim-to-correction timelines; comparison with viral spread timelines; analysis of whether corrective content achieves comparable virality to original claims',
    'If lag is always longer than amplification window: snare classification confirmed across more perspectives. If lag is reducing with distributed verification methods: scaffold perspective is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_production_lag, empirical, 'Minimum timeline for truth production vs virality').

omega_variable(
    epistemic_commons_regeneration,
    'Can decentralized information architectures (distributed systems, peer-to-peer verification, blockchain-based credibility) create alternative epistemic commons that resist fitness-truth decoupling?',
    'Empirical testing of alternative platforms; measurement of truth-tracking efficiency; analysis of whether decentralization eliminates or merely relocates the constraint',
    'If regenerable: scaffold perspective is confirmed (sunset via decentralized alternatives). If alternative systems develop their own distortions: constraint is isomorphic across architectures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_commons_regeneration, empirical, 'Whether decentralized architectures resolve the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(memetic_fitness_vs_truth, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(memfit_tr_t0, memetic_fitness_vs_truth, theater_ratio, 0, 0.35).
narrative_ontology:measurement(memfit_tr_t5, memetic_fitness_vs_truth, theater_ratio, 5, 0.58).
narrative_ontology:measurement(memfit_tr_t10, memetic_fitness_vs_truth, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(memfit_be_t0, memetic_fitness_vs_truth, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(memfit_be_t5, memetic_fitness_vs_truth, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(memfit_be_t10, memetic_fitness_vs_truth, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(memetic_fitness_vs_truth, information_standard).
narrative_ontology:affects_constraint(memetic_fitness_vs_truth, attention_capture_economics).
narrative_ontology:affects_constraint(memetic_fitness_vs_truth, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(memetic_fitness_vs_truth, epistemic_commons_degradation).

% DUAL FORMULATION NOTE:
% Memetic fitness vs truth decoupling is the parent constraint affecting downstream constraints: attention-capture economics depends on virality rewards, algorithmic bias emerges from engagement optimization, epistemic commons degradation results from fitness-truth decoupling. Each downstream constraint has higher ε reflecting their more severe extraction; upstream constraint (this one) represents the fundamental structural source.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(memetic_fitness_vs_truth, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
