% ============================================================================
% CONSTRAINT STORY: alignment_tax_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alignment_tax_tradeoff, []).

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
 *   constraint_id: alignment_tax_tradeoff
 *   human_readable: The Safety-Performance Divergence in AI Development
 *   domain: technological/AI/economic
 *
 * SUMMARY:
 *   The alignment-tax tradeoff represents a structural divergence in AI
 *   development between systems designed for maximum safety-alignment and
 *   those optimized for raw capability and performance. As AI systems become
 *   more consequential (financial decision-making, medical diagnosis,
 *   autonomous control), the cost of safety measures — interpretability
 *   layers, value verification mechanisms, robustness constraints,
 *   constitutional AI safeguards — imposes direct computational overhead and
 *   capability ceilings. In a competitive environment where market share,
 *   funding, and institutional prestige correlate with capability benchmarks,
 *   the safety-aligned developer faces a competitive disadvantage: their
 *   systems are slower, more resource-intensive, or less capable at core
 *   tasks. Simultaneously, the performance-optimized developer captures
 *   advantages: higher throughput, lower latency, superior capability
 *   metrics. This creates a Darwinian pressure toward lower-safety
 *   equilibrium. The constraint exhibits the full range of DR classifications
 *   depending on the observer's structural position: those forced to pay the
 *   alignment tax see extraction; those who benefit from capability races see
 *   coordination; regulators see a temporary problem with enforcement
 *   solutions; institutions with safety requirements see pure coordination;
 *   and analytical observers risk naturalizing what may be a contingent
 *   architectural choice as an immutable mathematical law. The extractiveness
 *   trajectory (0.32→0.58) reflects increasing market dominance by
 *   capability-optimized systems, deepening the divergence.
 *
 * KEY AGENTS:
 *   - Performance-Optimized Capability Labs: Primary beneficiary (institutional/arbitrage) — captures competitive advantage by deploying lower-safety, higher-capability systems; enjoys network effects and market dominance
 *   - Safety-Constrained Developers: Primary victim (powerless/trapped) — must implement alignment overhead that competitors avoid; face capability-disadvantage lock-in with no exit from competitive pressure
 *   - End-User Baseline Safety: Secondary victim (powerless/trapped) — exposed to less-aligned systems due to market preferences; lacks agency to enforce safety requirements in consumer markets
 *   - Alignment Research Community: Moderate victim (moderate/constrained) — benefits from safety-consciousness but lacks market power to enforce their standards; research findings subordinate to capability timelines
 *   - Regulatory and Standards Bodies: Organized enforcement actor (organized/constrained) — building verification infrastructure and safety standards (scaffold with sunset logic); depends on enforcement capacity and international coordination
 *   - Enterprise Safety Requirements: Institutional beneficiary (institutional/arbitrage) — can enforce safety requirements through procurement power; experiences constraint as coordination rather than extraction
 *   - Institutional AI Governance: Theater-maintaining actor (institutional/arbitrage) — ethics boards and review structures persist through legitimacy but lack enforcement capacity; piton classification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summit by naturalizing contingent architectural choices as fundamental verification limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alignment_tax_tradeoff, 0.58).
domain_priors:suppression_score(alignment_tax_tradeoff, 0.62).
domain_priors:theater_ratio(alignment_tax_tradeoff, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alignment_tax_tradeoff, extractiveness, 0.58).
narrative_ontology:constraint_metric(alignment_tax_tradeoff, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(alignment_tax_tradeoff, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alignment_tax_tradeoff, tangled_rope).
narrative_ontology:human_readable(alignment_tax_tradeoff, "The Safety-Performance Divergence in AI Development").
narrative_ontology:topic_domain(alignment_tax_tradeoff, "technological/AI/economic").

domain_priors:requires_active_enforcement(alignment_tax_tradeoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alignment_tax_tradeoff, performance_optimized_developers).
narrative_ontology:constraint_beneficiary(alignment_tax_tradeoff, capability_race_participants).
narrative_ontology:constraint_victim(alignment_tax_tradeoff, safety_constrained_systems).
narrative_ontology:constraint_victim(alignment_tax_tradeoff, alignment_research_community).
narrative_ontology:constraint_victim(alignment_tax_tradeoff, end_user_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAFETY-CONSTRAINED DEVELOPER (SNARE) — Faces maximum extraction. Implementing alignment safety measures (value verification layers, interpretability overhead, output filtering, constitutional AI principles) imposes direct computational cost and capability ceiling. In a capability-race dynamic, this developer cannot exit without abandoning market relevance. The safety tax is involuntary; alternatives are foreclosed. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END-USER BASELINE SAFETY (SNARE) — Cannot exit from exposure to AI systems. Derives value from alignment properties (interpretability, value-alignment, robustness to adversarial inputs) but trapped by market dynamics that incentivize speed and capability over safety. The constraint forces a choice between using unsafe high-capability systems or foregoing capability entirely. No real exit option.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALIGNMENT RESEARCH COMMUNITY (TANGLED ROPE) — Experiences mixed coordination and extraction. The constraint creates demand for alignment research (coordination benefit — safety verification, interpretability, value specification become valuable research domains). But constrained exit: alignment researchers lack market power to enforce standards; their findings are subordinate to capability timelines. The community benefits from problem salience but is extracted from by capability-race dynamics that deprioritize their work.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PERFORMANCE-OPTIMIZED LABS (ROPE) — Primary beneficiary with maximum exit optionality. Experiences the constraint as coordination: the safety tax creates a market gap (speed + capability at lower safety level) that they can exploit. Their arbitrage option is direct: deploy fast, collect network effects, establish market dominance before safety requirements can enforce. For these actors, the constraint is a coordination solution to a competitive problem.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY/STANDARDS BODIES (SCAFFOLD) — Organized agents (government AI offices, EU AI Act, ISO safety committees) view the alignment tax as a temporary coordination problem being solved by explicit enforcement: regulatory requirements for transparency, testing, and safety certification are building enforceable verification pathways. High suppression (regulatory authority) but with sunset logic — as standards mature and testing infrastructure advances, the extraction mechanism (unequal competitive advantage from ignoring safety) loses force. The constraint is enforcement-dependent with a decay trajectory.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ENTERPRISE SAFETY REQUIREMENTS (ROPE) — Large-scale institutional users (finance, healthcare, critical infrastructure) see safety-constrained AI as pure coordination: their business models depend on explainability, auditability, and liability containment. The alignment tax is a cost they willingly pay (or mandate their suppliers bear). Experienced as coordination rather than extraction because their exit option is explicit: they can enforce safety requirements through procurement standards.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INSTITUTIONAL GOVERNANCE (PITON) — Legacy AI governance structures (ethics boards, review committees, responsible AI programs) are substantially performative. These structures exist and consume institutional resources, but their functional capacity to enforce alignment properties across the competitive landscape is degraded — they lack authority over market-dominant actors and face persistent capability-race dynamics that override their recommendations. Governance persists through legitimacy theater, not functional enforcement. Theater ratio > 0.70.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FORMAL VIEW (MOUNTAIN) — From a universal mathematical perspective, there exists a fundamental tradeoff between expressiveness (capability) and verifiability (safety): as a system's decision space expands, the proof space required for complete alignment verification grows exponentially. Some alignment tax may be intrinsic to the structure of verification itself, not merely a contingent institutional artifact. However, the extent to which this formal limit is ACTIVE (versus merely theoretical) depends on architectural choices and verification methodology — this perspective risks false summit classification if it naturalizes what is actually a partially contingent engineering choice.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alignment_tax_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alignment_tax_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alignment_tax_tradeoff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alignment_tax_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alignment_tax_tradeoff, TR),
    TR >= 0.70.

:- end_tests(alignment_tax_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting genuine competitive advantage for performance-optimized developers and real cost burdens for safety-aligned systems. The tax is not absolute (some safety can be achieved without catastrophic overhead) but is material enough to affect market competition. The trajectory shows increasing divergence as capability scaling deepens verification requirements. Suppression (0.62): Moderately high. The competitive dynamics suppress alternatives to low-safety systems through market concentration, funding concentration toward capabilities research, and publication/prestige bias favoring breakthrough capabilities over incremental safety improvements. However, suppression is not total — regulatory mandates, institutional procurement standards, and safety-consciousness among researchers create countervailing pressure. Theater ratio (0.48): Moderate, slightly below the piton threshold. Institutional governance structures (ethics boards, safety reviews) exist and have some functional capacity, but their authority over market-dominant actors is degraded. However, unlike the fully-piton institutional governance perspective (where theater >> function), the regulatory scaffolds (EU AI Act, testing infrastructure) represent genuinely functional capacity being built, not pure theater. The theater ratio reflects the current state where governance is partly performative and partly functional, neither fully degraded (piton) nor fully effective (rope).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (computational overhead of alignment safety) produces radically different classifications depending on the observer's power, exit options, and time horizon. A performance-optimized lab experiences a market opportunity (Rope). A safety-constrained developer experiences an unsustainable cost burden (Snare). An alignment researcher experiences mixed coordination and extraction (Tangled Rope). A large institution with safety requirements experiences a coordination solution (Rope). A regulator experiences a temporary market failure being addressed through standards (Scaffold). Institutional governance sees its own ritualistic degradation (Piton). The analytical observer risks false summit by naturalizing institutional dynamics as mathematical necessity. None of these readings is 'wrong' — they are all structurally accurate from their respective positions. The perspectival gap reveals that the constraint's type IS NOT a fixed property but depends entirely on structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain operates as follows: (1) Base structural position: who benefits from high-capability systems (labs, end-users of capability-advantaged products) vs who bears the safety cost (safety-aligned developers, users dependent on low-capability systems, alignment researchers). (2) Exit options: performance labs can arbitrage their capability advantage globally (arbitrage exit = low d). Safety-constrained developers face capability-disadvantage lock-in with limited exit to other domains (trapped exit = high d). Alignment researchers can apply their skills elsewhere but face reduced market value for safety-focused work (constrained exit = medium-high d). (3) Power level affects f(d): powerless agents with trapped exits experience maximum f(d) ≈ 1.42, making χ high. Institutional agents with arbitrage exits experience f(d) ≈ -0.12, making χ negative (experienced as coordination). Moderate agents with constrained exits experience f(d) ≈ 0.75-1.00, making χ moderate. (4) Scope σ(S) at global scale (σ=1.2) amplifies extractiveness because the divergence is geographically distributed — high-capability systems from any jurisdiction capture network effects globally, making local safety investments less competitive. The directionality overrides are unnecessary here because the structural derivation produces correct values; the key is ensuring that beneficiaries and victims are correctly identified by their relationship to the extraction mechanism (who benefits from capability races, who bears the safety cost).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK: The alignment-tax tradeoff has high risk of mandatrophy — false categorization as pure extraction (Snare) when it contains genuine coordination function (Scaffold + some Rope elements). The risk arises from focusing exclusively on the safety-constrained developer's experience (genuine extraction burden) while neglecting the regulatory scaffold (Perspective 5) and institutional demand (Perspective 6) that create genuine coordination benefit for different actors. A purely snare reading would miss that the constraint is being actively solved by standards bodies, procurement requirements, and regulatory enforcement — these are not decorative but functionally building alternative verification pathways. However, the snare classification from the safety-developer perspective is not wrong; it is that perspective's genuine structural reality. MANDATROPHY RESOLUTION: The constraint is correctly classified as Tangled Rope at the system level because it exhibits: (A) Genuine coordination function — alignment research, safety standards, institutional procurement, regulatory frameworks are all solving real coordination problems (how to verify AI safety, how to maintain institutional liability control). (B) Genuine asymmetric extraction — performance-optimized developers extract competitive advantage precisely because safety-constrained developers cannot exit. (C) Active enforcement requirement — the constraint persists because market dynamics (capability race, funding concentration, prestige bias) actively suppress safety-aligned alternatives; regulatory scaffolds are REQUIRED to enforce higher-safety baselines. The mandatrophy is resolved by: (1) recognizing that the coordination function (safety verification, standards-setting) is real and creates legitimate value, not merely defensive reaction; (2) acknowledging that the extraction is also real — the capability race creates genuine lock-in preventing low-safety systems from shifting to high-safety; (3) noting that the constraint type varies by perspective (Snare for locked-in developers, Rope for labs, Scaffold for regulators) and the system-level type captures this variance through the Tangled Rope classification which includes all these perspectival readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_alignment_expressiveness_tradeoff,
    'Is the alignment safety tax fundamentally intrinsic to verification complexity, or is it primarily an artifact of current architectural choices and insufficient formal verification methods?',
    'Longitudinal analysis of alignment overhead (computational, latency, capability loss) across multiple architectural paradigms (language models, symbolic systems, hybrid approaches); comparison of verification methods (exhaustive testing vs formal proof vs empirical evaluation) and their overhead costs; historical analysis of similar tradeoff resolutions in other safety-critical domains (aviation, cryptography)',
    'If intrinsic: alignment tax is a mountain-like constraint on all advanced AI systems (non-negotiable cost of safety). If contingent: current alignment tax is substantially an institutional/architectural artifact that better methods could reduce below competitive-disadvantage threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_alignment_expressiveness_tradeoff, empirical, 'Whether alignment tax is fundamental to verification or contingent to current methods').

omega_variable(
    capability_race_irreversibility,
    'Once market dominance is established by a performance-optimized (lower-safety) system, can regulatory or technical standards enforcement successfully reset the competitive landscape toward higher-safety baselines?',
    'Historical case analysis of technology standards enforcement (emissions controls in automotive, network security standards in finance, privacy frameworks in data); modeling of market concentration dynamics and switching costs for entrenched systems; analysis of technical feasibility of retrofitting safety to deployed systems',
    'If irreversible: the safety-performance divergence locks in a low-safety equilibrium because early movers capture network effects and switching costs become prohibitive. Regulatory solutions cannot reset the game. If reversible: standards enforcement and switching mechanisms can force high-safety baselines even after low-safety capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_race_irreversibility, empirical, 'Whether capability race dominance creates irreversible safety lock-in').

omega_variable(
    alignment_cost_trajectory,
    'Does the computational overhead of alignment safety decrease over time (through algorithmic improvements and hardware efficiency), or does it increase as capability scaling requires deeper verification layers?',
    'Measurement of alignment overhead (FLOPS, latency, capability loss) for equivalent safety levels across time periods; correlation with hardware advances, algorithm improvements, and capability scaling; comparison of overhead trends in safety-critical systems with similar scaling dynamics',
    'If overhead decreases: the alignment tax becomes progressively cheaper, reducing competitive disadvantage and enabling high-safety market equilibrium. If overhead increases: safety becomes progressively more expensive relative to capability, deepening the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_cost_trajectory, empirical, 'Trajectory of alignment safety computational overhead over time').

omega_variable(
    international_coordination_feasibility,
    'Can international coordination mechanisms (treaties, mutual enforcement agreements, supply-chain verification) enforce globally consistent safety standards despite competitive incentives to defect?',
    'Analysis of precedent cases (nonproliferation treaties, export controls, environmental agreements); modeling of defection incentives and verification costs; assessment of enforcement mechanisms and reciprocal consequences',
    'If feasible: regulatory scaffold (Perspective 5) is structurally sound and can converge toward high-safety global baseline. If infeasible: defection incentives are strong enough to undermine coordination, locking in low-safety equilibrium despite intentions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_coordination_feasibility, empirical, 'Whether international safety coordination can overcome competitive defection incentives').

omega_variable(
    user_preference_heterogeneity,
    'What proportion of end-user demand actually prefers safety-aligned AI over raw capability, and does this demand segment remain distinct or merge with mass-market capability preferences?',
    'Market research on willingness-to-pay for safety properties vs capability; segmentation analysis of institutional vs consumer preferences; longitudinal tracking of demand shifts as AI capabilities become more consequential',
    'If robust demand: safety-preferred market segment can sustain high-safety suppliers despite capability-race dynamics, reducing extraction mechanism. If merged demand: safety becomes a luxury good only affordable to institutional buyers, deepening the divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_preference_heterogeneity, empirical, 'Market demand for safety-aligned AI vs raw capability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alignment_tax_tradeoff, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aln_tax_tr_t0, alignment_tax_tradeoff, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aln_tax_tr_t3, alignment_tax_tradeoff, theater_ratio, 3, 0.42).
narrative_ontology:measurement(aln_tax_tr_t6, alignment_tax_tradeoff, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(aln_tax_be_t0, alignment_tax_tradeoff, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(aln_tax_be_t3, alignment_tax_tradeoff, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(aln_tax_be_t6, alignment_tax_tradeoff, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alignment_tax_tradeoff, enforcement_mechanism).
narrative_ontology:affects_constraint(alignment_tax_tradeoff, ai_capability_scaling_constraint).
narrative_ontology:affects_constraint(alignment_tax_tradeoff, verification_bottleneck).
narrative_ontology:affects_constraint(alignment_tax_tradeoff, market_dominance_lock_in).

% DUAL FORMULATION NOTE:
% The alignment-tax tradeoff decomposes into two related but structurally distinct constraints: (1) FUNDAMENTAL: the mathematical relationship between expressiveness and verifiability (potential mountain/rope depending on architectural choices), (2) INSTITUTIONAL: the competitive dynamics that amplify the fundamental cost into market-significant disadvantage (definitely tangled_rope due to asymmetric extraction). This story addresses the institutional divergence (ε=0.58); a separate story on the formal verification complexity (lower ε) would complete the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alignment_tax_tradeoff, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
