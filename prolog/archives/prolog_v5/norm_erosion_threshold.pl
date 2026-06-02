% ============================================================================
% CONSTRAINT STORY: norm_erosion_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_norm_erosion_threshold, []).

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
 *   constraint_id: norm_erosion_threshold
 *   human_readable: The Social Fabric Breach
 *   domain: social/political
 *
 * SUMMARY:
 *   The social fabric breach models the non-linear breakdown of cooperation
 *   when perceived or actual norm defection crosses a critical threshold.
 *   Below the threshold, cooperative systems maintain themselves through a
 *   combination of internalized norms (low theater), distributed enforcement
 *   (family, community reputation), and selective punishment of outliers. At
 *   or beyond the threshold, cooperation becomes fragile: enforcement
 *   institutions must escalate dramatically to maintain compliance, or
 *   cooperation collapses entirely. The constraint exhibits six distinct
 *   classifications from different structural positions: the cooperative
 *   majority experiences it as a snare (trapped, no exit); strategic
 *   defectors experience it as pure coordination (norm system enables their
 *   gains); enforcement institutions experience it as tangled rope (benefit
 *   from the enforcement burden while claiming to solve it); traditional norm
 *   internalization appears as a degraded piton (performative rituals
 *   maintained by inertia); community resilience builders see it as temporary
 *   scaffolding with a sunset clause; and the civilizational analytical view
 *   risks naturalizing it as an immutable threshold (mountain). The theater
 *   ratio reflects the increasing performative content of enforcement as
 *   actual compliance mechanisms (internalization, community pressure)
 *   atrophy.
 *
 * KEY AGENTS:
 *   - Cooperative Majority: Primary victim (powerless/trapped) — must accept escalating enforcement costs or face norm system collapse
 *   - Strategic Defectors: Primary beneficiary (powerful/arbitrage) — benefit from exploiting norm system; selective rule-breaking in low-enforcement domains
 *   - Norm Enforcement Institutions: Institutional beneficiary (organized/constrained) — gain power and budget justification from elevated defection; extract through enforcement escalation
 *   - Traditional Norm Internalization: Institutional actor (institutional/constrained) — degraded mechanism maintained by inertia; performative rituals with low actual function
 *   - Community Resilience Builders: Moderate organized agents (moderate/mobile) — implement temporary interventions with sunset logic; succeed only if they prevent threshold breach
 *   - Collective Trust Capital: Abstract victim (powerless/trapped) — the epistemic commons bearing cost of norm erosion; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(norm_erosion_threshold, 0.58).
domain_priors:suppression_score(norm_erosion_threshold, 0.65).
domain_priors:theater_ratio(norm_erosion_threshold, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(norm_erosion_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(norm_erosion_threshold, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(norm_erosion_threshold, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(norm_erosion_threshold, tangled_rope).
narrative_ontology:human_readable(norm_erosion_threshold, "The Social Fabric Breach").
narrative_ontology:topic_domain(norm_erosion_threshold, "social/political").

domain_priors:requires_active_enforcement(norm_erosion_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(norm_erosion_threshold, norm_enforcement_institutions).
narrative_ontology:constraint_beneficiary(norm_erosion_threshold, defecting_subgroups).
narrative_ontology:constraint_victim(norm_erosion_threshold, cooperative_majority).
narrative_ontology:constraint_victim(norm_erosion_threshold, collective_trust_capital).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COOPERATIVE MAJORITY (SNARE) — Trapped by tipping point dynamics. Once defection rates exceed critical threshold, cooperators face escalating extraction: they must either increase enforcement costs (surveillance, punishment, institutional complexity) or accept norm collapse. No exit: cannot relocate from society, cannot opt out of norm system. Bears full cost of threshold breach without choice or compensation.
constraint_indexing:constraint_classification(norm_erosion_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STRATEGIC DEFECTORS (ROPE) — Benefit from coordination paradox: norm system enables their defection by providing predictable target population. Experience constraint as pure coordination — the norm framework makes defection gains extractable. Arbitrage options allow selective rule-breaking in low-enforcement domains.
constraint_indexing:constraint_classification(norm_erosion_threshold, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: NORM ENFORCEMENT INSTITUTIONS (TANGLED ROPE) — Primary beneficiary and primary enforcer. Gain institutional power and resource justification from elevated defection rates ('need more enforcement'). Simultaneously constrained by escalating cost of maintaining compliance as threshold approaches. Mixed: benefit from the problem they claim to solve; extraction disguised as coordination.
constraint_indexing:constraint_classification(norm_erosion_threshold, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADITIONAL NORM INTERNALIZATION (PITON) — The pre-threshold enforcement mechanism (family, church, community reputation) has atrophied in modern societies. Institutional inertia maintains vestigial rituals (ceremony, public shaming, formal censure) that are largely performative. Theater ratio high (0.48 baseline, rising). The mechanism persists because alternatives haven't fully replaced it, not because internalization still works. Degraded but inertial.
constraint_indexing:constraint_classification(norm_erosion_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMMUNITY RESILIENCE BUILDERS (SCAFFOLD) — Moderate agents implementing temporary interventions: mutual aid networks, local accountability systems, restorative justice. Experience the constraint as a coordination problem with a sunset clause. The intervention succeeds only if it prevents threshold breach — if successful, enforcement burden decreases and the scaffolding becomes unnecessary. Low effective extraction because the mechanism's goal is its own obsolescence.
constraint_indexing:constraint_classification(norm_erosion_threshold, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CRITICAL THRESHOLD VIEW (MOUNTAIN) — From civilizational/universal timescale, the constraint appears as a fundamental property of cooperation dynamics: any system based on mutual restraint has a mathematical tipping point beyond which cooperation becomes unrecoverable. Presents itself as natural law. However, the structural data (active enforcement, beneficiaries, victims) contradicts mountain classification — the engine will identify this as false summit, revealing that the 'inevitable tipping point' naturalizes contingent institutional arrangements and enforcement choices.
constraint_indexing:constraint_classification(norm_erosion_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(norm_erosion_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(norm_erosion_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(norm_erosion_threshold, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(norm_erosion_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(norm_erosion_threshold, TR),
    TR >= 0.70.

:- end_tests(norm_erosion_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint shows significant extraction dynamics as enforcement institutions scale up, and strategic defectors capture gains from norm-system exploitation. However, at t=0 (below-threshold state), extractiveness is lower (0.28) because traditional internalization does most of the work with minimal institutional overhead. As the threshold approaches, extractiveness rises (0.42 at t=5, 0.58 at t=10) because enforcement costs escalate and the system transitions from self-maintaining (low theater, internalized norms) to administratively intensive (high theater, institutional policing). Suppression (0.65): Moderate-high. Significant barriers exist to exiting or opting out of norm systems (cannot relocate from society), and defection risk is both socially and institutionally punished. But suppression is not total — some subgroups maintain parallel norm systems, and defection rates suggest non-zero exit/adaptation capacity. Theater ratio (0.48 → rising): Moderate baseline increasing. Traditional enforcement (family shame, community reputation) is nearly zero-theater — internalization is genuine functional compliance. Modern enforcement (courts, police, surveillance) is substantially performative — expensive administrative rituals whose actual compliance effect is contested. The theater ratio rising reflects the substitution of performative institutional enforcement for genuine internalization.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon (norm defection rate crossing a critical threshold) produces opposite classifications and opposite experienced extractiveness across agent positions. The cooperative majority sees a snare: escalating extraction with no exit. Enforcement institutions see rope or tangled rope: coordination benefit plus enforcement power. Strategic defectors see pure coordination: the system enables their gains. The analytical observer risks seeing inevitability (mountain) when the structural data shows contingency (tangled rope at best, snare for victims). The perspectival gap is maximal here because the constraint involves a tipping point: below threshold, many agents experience it as rope (pure coordination, fair exchange); above threshold, the same agents experience it as snare (extraction without exit). The classification changes discontinuously with the observer's threshold position.
 *
 * DIRECTIONALITY LOGIC:
 *   Norm enforcement institutions occupy the paradoxical position of primary beneficiary AND primary enforcer. Their directionality (d) is high because they are institutional beneficiaries of the enforcement burden (arbitrage exit options, power accumulation). But they are simultaneously constrained by the escalating costs of maintaining compliance as defection rates rise. The derived d reflects this: beneficiaries with arbitrage options normally get low/negative f(d), but the tangled rope gate requires BOTH beneficiary status AND active enforcement AND victims, which produces the hybrid classification. The cooperative majority gets high d (high f(d) → high χ) because they are trapped with no exit and victims of the enforcement escalation. Strategic defectors get low d despite being beneficiaries because their arbitrage exit options (selective rule-breaking in low-enforcement domains) reduce their experienced extraction. Community resilience builders get moderate d because they have mobile exit options and are not primarily victims — they are third-party interveners.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by showing that the 'inevitable threshold' framing (mountain) naturalizes what are actually institutional choices about enforcement intensity, visibility of defection, and scope of norm systems. The constraint is NOT a fundamental law of cooperation dynamics — it is a contingent feature of specific enforcement architectures. Societies with heterogeneous norm systems, low-visibility defection rates, or decentralized enforcement may avoid sharp thresholds entirely. The analytical observer's mountain is a false summit produced by aggregating heterogeneous systems into a single abstract model. The structure revealed is tangled rope (enforcement institutions benefit from the burden while claiming to solve it) overlaid on snare (cooperative majority trapped by escalation dynamics). The scaffold perspective (community resilience) is the only pathway that actually resolves the constraint rather than managing it — by preventing threshold breach through distributed enforcement and early intervention, scaffolding makes itself unnecessary. This is the hallmark of legitimate coordination: the solution succeeds when it solves the problem, not when it perpetuates it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_defection_threshold_location,
    'What is the empirical critical defection rate (percentage of population visibly breaking primary norms) that triggers nonlinear cooperation collapse?',
    'Historical analysis of regime transitions, community collapse events, and civil conflict onset; network models of norm cascade dynamics with varying parameters',
    'If threshold < 10%: many societies are already beyond recovery point. If threshold 10-20%: near-term policy windows exist. If threshold > 30%: substantial stability margin suggests current framing exaggerates risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_defection_threshold_location, empirical, 'The critical defection rate triggering cooperation collapse').

omega_variable(
    perception_vs_reality_gap,
    'Does the actual defection rate drive threshold collapse, or does the PERCEIVED defection rate drive it? Can perception management substitute for actual norm compliance?',
    'Comparison of collapse timing with actual defection rates vs media/institutional reporting; studies of communities where actual compliance high but perception low (and vice versa); experimental evidence on threshold response to visibility cues',
    'If perception drives collapse: the constraint is substantially a coordination problem on information (Rope/Scaffold). If actual rate drives it: the constraint is pure dynamics (Mountain or Snare depending on causation). This distinction determines whether enforcement interventions should target behavior or narratives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(perception_vs_reality_gap, empirical, 'Whether perception or actual defection rate drives threshold collapse').

omega_variable(
    heterogeneous_norm_systems,
    'Do societies with nested, heterogeneous norm systems (subgroups with different enforcement regimes) avoid the tipping point that monolithic norm systems reach?',
    'Cross-cultural comparison of norm diversity vs collapse risk; case studies of polycultural societies that maintained cooperation stability across threshold-risk periods; network models of heterogeneous enforcement',
    'If heterogeneity confers stability: the tipping point is an artifact of centralized enforcement (Snare/Tangled Rope). If heterogeneity is irrelevant: the threshold is structural to any cooperation system (Mountain or Rope with catastrophic failure mode).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heterogeneous_norm_systems, conceptual, 'Whether heterogeneous norm systems avoid critical threshold collapse').

omega_variable(
    enforcement_cost_escalation_loop,
    'Does the extractiveness of enforcement institutions systematically increase as defection rates rise, creating a feedback loop that amplifies the threshold effect?',
    'Time series analysis of enforcement budgets, prison populations, surveillance capacity during periods of rising defection; mechanism analysis showing whether enforcement costs scale linearly or super-linearly; comparison of societies with different enforcement escalation rates',
    'If enforcement creates positive feedback: the constraint is Tangled Rope (extraction disguised as coordination). If enforcement costs remain proportional to defection: the constraint is pure dynamics (Mountain). This determines whether reducing enforcement institutions can stabilize cooperation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_cost_escalation_loop, empirical, 'Whether enforcement cost escalation creates a positive feedback loop').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(norm_erosion_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(norm_tr_t0, norm_erosion_threshold, theater_ratio, 0, 0.32).
narrative_ontology:measurement(norm_tr_t5, norm_erosion_threshold, theater_ratio, 5, 0.4).
narrative_ontology:measurement(norm_tr_t10, norm_erosion_threshold, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(norm_be_t0, norm_erosion_threshold, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(norm_be_t5, norm_erosion_threshold, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(norm_be_t10, norm_erosion_threshold, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(norm_erosion_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(norm_erosion_threshold, trust_capital_depletion).
narrative_ontology:affects_constraint(norm_erosion_threshold, institutional_legitimacy_crisis).
narrative_ontology:affects_constraint(norm_erosion_threshold, collective_action_capacity_collapse).

% DUAL FORMULATION NOTE:
% The norm erosion threshold is downstream of individual defection decisions but represents a distinct structural constraint on collective cooperation. Upstream constraints model micro-level incentives to defect; this constraint models macro-level threshold dynamics of norm system collapse. The network links show causal dependencies: norm erosion accelerates trust depletion, which accelerates institutional legitimacy loss, which accelerates collective action failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(norm_erosion_threshold, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
