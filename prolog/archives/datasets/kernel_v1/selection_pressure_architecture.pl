% ============================================================================
% CONSTRAINT STORY: selection_pressure_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_selection_pressure_architecture, []).

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
 *   constraint_id: selection_pressure_architecture
 *   human_readable: Selection Pressure as Architecture: Optimization Environments Select Against Verification
 *   domain: epistemology/information_theory/institutional
 *
 * SUMMARY:
 *   Selection pressure as architecture describes a structural property that
 *   emerges whenever an information environment optimizes for one metric
 *   while leaving another metric unmeasured. The constraint is not about bad
 *   actors, poor design, or malice — it is about the logic of optimization
 *   itself. If an environment optimizes for signal propagation (reach, speed,
 *   engagement, citation volume, trading velocity) and does not include
 *   truth-value verification in the objective function, the optimization
 *   process will find strategies that maximize propagation while truth-value
 *   drifts as a side effect. This is not a law of physics. It is a law of
 *   optimization: the system optimizes what it measures. The constraint has
 *   become more severe over the interval (0-20) as optimization environments
 *   have scaled (from pre-internet expert communities to planetary-scale
 *   algorithmic feeds) and as computational capacity to search the
 *   optimization landscape has increased. The trajectory shows rising
 *   extractiveness (optimization pressure intensifying), rising theater (peer
 *   review and verification language persisting while functional verification
 *   declines), and rising suppression (barriers to exit from
 *   propagation-optimized systems increasing as institutional entrenchment
 *   deepens). The constraint demonstrates all six DR types from different
 *   perspectives, revealing that classification depends entirely on the
 *   observer's structural position relative to the optimization flow.
 *
 * KEY AGENTS:
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good of information accuracy that cannot organize, fund itself, or exit
 *   - Verification Agents: Secondary victim (powerless/trapped) — researchers, fact-checkers, auditors selected against by optimization pressure; career and resource disadvantage
 *   - Signal Propagators: Primary beneficiary (institutional/arbitrage) — media systems, algorithmic feeds, publish-or-perish academia, trading platforms that profit from propagation optimization
 *   - Individual Communicators: Secondary beneficiary (moderate/constrained) — researchers, journalists, public intellectuals who benefit from reach but are selected to distort accuracy
 *   - System Designers: Powerful actor (powerful/mobile) — algorithm architects and incentive structure creators who could change objective functions but face institutional pressure
 *   - Verification Infrastructure: Organized actor (organized/constrained) — fact-checkers, registered reports, open-science movements building alternative pathways
 *   - Peer Review Institution: Degraded actor (institutional/arbitrage) — maintains verification theater while functional verification declines due to optimization pressure
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent optimization choices as laws of information physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(selection_pressure_architecture, 0.62).
domain_priors:suppression_score(selection_pressure_architecture, 0.58).
domain_priors:theater_ratio(selection_pressure_architecture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(selection_pressure_architecture, extractiveness, 0.62).
narrative_ontology:constraint_metric(selection_pressure_architecture, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(selection_pressure_architecture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(selection_pressure_architecture, tangled_rope).
narrative_ontology:human_readable(selection_pressure_architecture, "Selection Pressure as Architecture: Optimization Environments Select Against Verification").
narrative_ontology:topic_domain(selection_pressure_architecture, "epistemology/information_theory/institutional").

domain_priors:requires_active_enforcement(selection_pressure_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(selection_pressure_architecture, signal_propagators).
narrative_ontology:constraint_beneficiary(selection_pressure_architecture, optimization_system_designers).
narrative_ontology:constraint_victim(selection_pressure_architecture, epistemic_commons).
narrative_ontology:constraint_victim(selection_pressure_architecture, verification_agents).
narrative_ontology:constraint_victim(selection_pressure_architecture, information_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The abstract collective good of information accuracy cannot organize, cannot exit, and bears the accumulated cost of optimization-driven signal distortion. No agent advocates for it; no constituency funds it. The epistemic commons experiences maximum extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(selection_pressure_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: VERIFICATION AGENT (SNARE) — Agents tasked with verifying signal accuracy are structurally disadvantaged: verification is computationally expensive, produces negative rather than positive results (disconfirming rather than confirming), and is selected against by any optimization environment that rewards signal volume or speed. A verification agent faces systematic career penalty and resource constraint. Exit is impossible without abandoning the verification role entirely.
constraint_indexing:constraint_classification(selection_pressure_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SIGNAL PROPAGATOR (ROPE) — Institutional actors optimizing for signal propagation (media systems, algorithmic feeds, academic publish-or-perish incentives, trading systems) experience the constraint as pure coordination: the optimization function they follow naturally selects for accessibility, speed, and resonance. They benefit from the architecture because their success is measured by the same metrics the architecture optimizes. Arbitrage escape is always available: abandon the optimization and pursue truth-seeking instead (which they could do but do not).
constraint_indexing:constraint_classification(selection_pressure_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIVIDUAL COMMUNICATOR (TANGLED ROPE) — A researcher, journalist, or public intellectual faces both the coordination benefit (reach, audience growth, career advancement through the optimization architecture) and the extraction cost (incentive to simplify, dramatize, or omit caveats to maximize signal propagation). They benefit from the system's propagation mechanism and are simultaneously selected to distort their own accuracy. Exit is costly (reduced reach, smaller audience, career damage) but possible through independent platforms or niche communities. Moderate experienced extraction reflects their partial agency.
constraint_indexing:constraint_classification(selection_pressure_architecture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VERIFICATION INFRASTRUCTURE MOVEMENT (SCAFFOLD) — Organized agents (fact-checkers, registered reports, preprint review communities, open-science advocates, decentralized reputation systems) build parallel verification pathways that operate on different optimization metrics: accuracy-first rather than propagation-first. These infrastructures are temporary scaffolds — they require constant effort maintenance and external funding because they operate against the default optimization pressure. They have sunset logic: as verification becomes institutionalized (integrating into academic hiring, media credibility metrics, algorithmic ranking), the scaffold transitions to rope. Current state: constrained by limited resources and institutional resistance.
constraint_indexing:constraint_classification(selection_pressure_architecture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PEER REVIEW SYSTEM (PITON) — Traditional peer review was designed to verify accuracy but is now selected against by optimization pressure: it is slow (incompatible with propagation speed), expensive (incompatible with volume metrics), and produces binary reject/accept decisions (incompatible with algorithmic ranking). The system persists through institutional inertia and legitimacy theater, not functional verification. Theater ratio: journals maintain verification language while the actual selection mechanism has shifted to propagation metrics (citation count, impact factors, journal prestige based on aggregated signal, not accuracy). The institution acknowledges its degradation (constant crisis narratives about peer review's failures) but maintains the ritual.
constraint_indexing:constraint_classification(selection_pressure_architecture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a information-theoretic perspective, any optimization environment that does not explicitly include truth-value verification in its objective function will necessarily select for high-signal-volume strategies. This is not contingent on human intentions — it is a property of optimization itself. The observation resembles a theorem: given an optimization landscape where accessibility/speed/propagation are tracked and truth-value is not, the optimizer will find paths that maximize these dimensions. This perspective risks naturalizing what is actually a choice about objective function design. The engine will likely identify this as a false summit: the 'structure of optimization' framing naturalizes what is a contingent institutional choice to optimize for propagation rather than accuracy.
constraint_indexing:constraint_classification(selection_pressure_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: SYSTEM DESIGNER (TANGLED ROPE) — Architects of optimization systems (algorithm designers, platform engineers, incentive structure creators) experience the constraint as genuine coordination problem with embedded extraction. Coordination: they must design systems that propagate information and enable communication. Extraction: the optimization metrics available to them (engagement, reach, velocity) naturally select against verification. They could change the objective function (include accuracy, include uncertainty quantification, include disconfirming evidence) but face institutional pressure and resource constraints. Mobile exit exists but is costly: switching to accuracy-first optimization often requires rejecting profit incentives, institutional support, and user base. Experienced extraction is real but not maximal because they retain agency in metric selection.
constraint_indexing:constraint_classification(selection_pressure_architecture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(selection_pressure_architecture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(selection_pressure_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(selection_pressure_architecture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(selection_pressure_architecture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(selection_pressure_architecture, TR),
    TR >= 0.70.

:- end_tests(selection_pressure_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts significant value from verification agents and the epistemic commons (they bear costs without benefiting), while benefiting signal propagators and individual communicators who ride the optimization wave. The value is not maximal (0.72+) because system designers retain some agency — they could shift metrics and do so in some contexts (academic hiring reforms, platform accuracy initiatives). The trajectory shows rising extractiveness (0-20: 0.35→0.62) indicating the optimization environment's increasing maturity and computational capacity to find propagation-maximizing strategies. Suppression (0.58): Moderate-high. Significant barriers to exit from propagation-optimized systems include: (1) institutional entrenchment (most communication channels are optimized for propagation), (2) career costs (switching to accuracy-first communication reduces reach and funding), (3) technical infrastructure (algorithmic ranking, recommendation systems, engagement metrics are built into platforms), (4) user base lock-in (leaving the optimization environment means losing audience). These are real but not total — niche communities and independent platforms exist and function, though at reduced scale. Theater ratio (0.68): High. The constraint exhibits high theater because verification language persists (peer review, fact-checking, accuracy claims) while the actual selection mechanism has shifted to propagation metrics. Journals claim verification while their prestige derives from propagation (citation counts, impact factors based on volume not accuracy). The theater has risen over the interval (0-20: 0.45→0.68) as optimization environments have scaled and as the gap between verification rhetoric and propagation reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across the observation site. The epistemic commons sees pure extraction and immobility (Snare). Verification agents see the same extraction and immobility (Snare). Signal propagators see coordination without extraction (Rope) — they are solving a real problem of communicating information. System designers see mixed coordination and extraction (Tangled Rope) — they need to propagate signal but are selected to distort accuracy. Verification infrastructure sees a temporary problem with a sunset (Scaffold) — alternative pathways are buildable but require sustained effort. Peer review sees its own degradation (Piton) — performing verification ritual without functional verification. The analytical observer risks seeing a law of information physics (Mountain) — optimization necessarily selects for whatever is measured. The gap reflects that the constraint is not experienced uniformly: those riding the optimization wave (signal propagators) see coordination; those opposing it (verification agents) see extraction; those designing it (system designers) see both. No single type captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d (from 0.0 = full beneficiary to 1.0 = full target) is determined by each agent's structural position in the optimization flow. Signal propagators have d ≈ 0.1-0.2 (beneficiaries with arbitrage escape): they profit from the optimization environment and could choose accuracy-first metrics but do not. Verification agents have d ≈ 0.95 (full targets): they oppose the optimization flow and cannot escape it without abandoning verification. Epistemic commons has d ≈ 1.0 (extreme target): abstract good that cannot organize or exit. Individual communicators have d ≈ 0.55-0.60 (both beneficiary and victim): they benefit from reach but are selected to distort. System designers have d ≈ 0.40-0.50 (moderate targets): they could change the metrics but face institutional pressure. Verification infrastructure has d ≈ 0.65-0.70 (mostly target): they oppose the dominant optimization but have agency through organized action. The engine derives d from beneficiary/victim declarations and exit options; this perspective explicitly traces how structural position maps to experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint exhibits genuine coordination (signal propagation is a real problem requiring institutional solution) alongside genuine extraction (the solution selects against verification). The Tangled Rope classification (base properties: extractiveness 0.62, suppression 0.58, beneficiaries + victims + enforcement) correctly captures both dimensions. The false-summit risk (the mountain perspective naturalizing optimization as law) is mitigated by the structural data showing beneficiaries (signal propagators) whose interests depend on maintaining the propagation optimization. If the constraint were truly natural law, no beneficiaries would exist — the beneficiary presence reveals the constraint as constructed institutional choice. The mandatrophy resolves to: this is a hybrid constraint with genuine coordination function and asymmetric extraction, classified as Tangled Rope with false-summit risk flagged for analytical observers who might naturalize the optimization logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truth_value_formalization,
    'Is truth-value formally formalizable as an objective function for optimization environments?',
    'Epistemological analysis and formal systems exploration: examine whether truth-value can be encoded as a computable metric analogous to engagement or reach; test whether ground-truth verification enables automated truth-seeking optimization',
    'If formalizable: the constraint becomes a choice problem (designers select propagation over truth metrics). If not formalizable: the constraint resembles a natural law (optimization cannot include what cannot be formalized). Classification impact: Rope/Tangled Rope vs Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_value_formalization, conceptual, 'Whether truth-value can be formalized as an objective function').

omega_variable(
    optimization_necessity,
    'Is optimization for signal propagation a necessary feature of communication systems or a contingent institutional choice?',
    'Historical comparison of communication architectures: societies that prioritized accuracy metrics over propagation metrics; modern systems that implement accuracy-first optimization (e.g., Cochrane for medical evidence, PubPeer for research verification). Analysis of whether propagation optimization is driven by technical constraints or institutional incentives (profit, reach, influence).',
    'If necessary: the constraint is architectural (mountain). If contingent: the constraint is institutional design (tangled rope/snare). This omega directly affects whether false-summit detection applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimization_necessity, empirical, 'Whether propagation optimization is necessary or contingent').

omega_variable(
    verification_burden_distribution,
    'Can verification costs be distributed or absorbed without creating powerless trapped agents?',
    'Design analysis of verification systems: explore whether verification can be embedded as a low-cost component of propagation (e.g., automated confidence scoring, uncertainty quantification, preprint scrutiny layers) or whether verification always requires dedicated high-cost agents. Test whether distributed verification (many-eyes) produces equivalent accuracy to centralized verification.',
    'If distributeable and low-cost: verification can be integrated into the propagation architecture (Rope result). If verification always requires dedicated high-cost agents: the snare aspect (powerless verification agents) is structural and unavoidable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_burden_distribution, empirical, 'Whether verification burden can be distributed without creating trapped powerless agents').

omega_variable(
    objective_function_malleability,
    'Can optimization systems meaningfully shift toward accuracy-first metrics without collapsing propagation functionality?',
    'Field experiments in platform redesign, academic incentive restructuring, and algorithmic metric weighting. Measure whether systems that prioritize accuracy-first (Cochrane, arXiv, PubPeer, registered reports) achieve adequate signal propagation or suffer adoption/reach collapse. Assess whether accuracy and propagation can coexist as weighted objectives.',
    'If coexistence is possible: system designers have real agency (tangled rope, not snare). If accuracy-first causes propagation collapse or vice versa: the constraint is genuinely structural (mountain, snare, or rope depending on perspective). This omega informs whether the scaffold perspective is realistic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(objective_function_malleability, empirical, 'Whether accuracy-first optimization can coexist with propagation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(selection_pressure_architecture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spa_tr_t0, selection_pressure_architecture, theater_ratio, 0, 0.45).
narrative_ontology:measurement(spa_tr_t10, selection_pressure_architecture, theater_ratio, 10, 0.58).
narrative_ontology:measurement(spa_tr_t20, selection_pressure_architecture, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(spa_be_t0, selection_pressure_architecture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spa_be_t10, selection_pressure_architecture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(spa_be_t20, selection_pressure_architecture, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spa_su_t0, selection_pressure_architecture, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(spa_su_t10, selection_pressure_architecture, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(spa_su_t20, selection_pressure_architecture, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(selection_pressure_architecture, information_standard).
narrative_ontology:affects_constraint(selection_pressure_architecture, verification_bottleneck).
narrative_ontology:affects_constraint(selection_pressure_architecture, publish_or_perish_extraction).
narrative_ontology:affects_constraint(selection_pressure_architecture, algorithmic_ranking_bias).

% DUAL FORMULATION NOTE:
% Selection pressure architecture is the upstream constraint affecting specific downstream manifestations (academic publishing, media algorithms, trading systems). The upstream constraint describes the general optimization property; downstream constraints describe domain-specific instantiations. All are linked by the same mechanism: optimization for propagation without truth-value in the objective function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(selection_pressure_architecture, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
