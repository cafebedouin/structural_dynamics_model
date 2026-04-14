% ============================================================================
% CONSTRAINT STORY: network_cold_start_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_cold_start_problem, []).

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
 *   constraint_id: network_cold_start_problem
 *   human_readable: Network Cold Start Problem
 *   domain: economics/platform_markets/coordination
 *
 * SUMMARY:
 *   The network cold start problem describes the structural asymmetry created
 *   when a platform operator bootstraps a two-sided or multi-sided market
 *   from zero liquidity. Early participants enjoy scarce supply and operator
 *   subsidies; they coordinate network growth. Late participants arrive into
 *   a market where network effects have already created switching costs and
 *   liquidity advantages for incumbents. The constraint exhibits mixed
 *   properties: genuine coordination problem (how to achieve critical mass
 *   when value depends on participation scale) overlaid with asymmetric
 *   extraction (early participants and the operator capture disproportionate
 *   value from network effects lock-in). Theater is moderate-high: political
 *   discourse frames network dominance as natural law ('network effects are
 *   inevitable') while strategic design choices (algorithmic ranking, data
 *   blocking, payment system gatekeeping) actively construct and reinforce
 *   lock-in. The extractiveness trajectory shows accumulation: early in the
 *   network's lifecycle, extraction is low (everyone is participating for
 *   utility, not for lock-in advantage). As the network approaches and passes
 *   critical mass, extractiveness rises sharply as switching costs lock in
 *   latecomer participants and alternative networks become uncompetitive. By
 *   year 6 (the endpoint), the original coordination problem is 'solved' —
 *   the network has scale — but extraction mechanisms (network effects,
 *   switching costs) have crystallized.
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — solves cold start problem, captures value from network effects and switching cost lock-in
 *   - Early Adopters: Secondary beneficiary (powerful/mobile) — enjoy preferential access, operator subsidies, liquidity advantages during critical growth phase
 *   - Latecomer Participants: Primary victim (powerless/trapped) — locked into network after tipping point; bear switching costs and liquidity disadvantages; cannot exit despite degraded value proposition
 *   - Alternative Network Projects: Secondary victim (moderate/constrained) — face network effect headwinds; cannot bootstrap critical mass once dominant network has tipped; face asymmetric extraction through talent and capital flow to incumbent
 *   - Regulatory Intervention Coalition: Organized intervenor (organized/constrained) — advocates for interoperability mandates and data portability regulations to lower switching costs and create genuine exit options
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent design choices (algorithmic ranking, data blocking) as immutable network laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_cold_start_problem, 0.52).
domain_priors:suppression_score(network_cold_start_problem, 0.58).
domain_priors:theater_ratio(network_cold_start_problem, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_cold_start_problem, extractiveness, 0.52).
narrative_ontology:constraint_metric(network_cold_start_problem, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(network_cold_start_problem, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_cold_start_problem, tangled_rope).
narrative_ontology:human_readable(network_cold_start_problem, "Network Cold Start Problem").
narrative_ontology:topic_domain(network_cold_start_problem, "economics/platform_markets/coordination").

domain_priors:requires_active_enforcement(network_cold_start_problem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_cold_start_problem, platform_operator).
narrative_ontology:constraint_beneficiary(network_cold_start_problem, early_adopters).
narrative_ontology:constraint_victim(network_cold_start_problem, latecomer_participants).
narrative_ontology:constraint_victim(network_cold_start_problem, alternative_network_projects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATECOMER PARTICIPANT (SNARE) — Locked into network after tipping point due to coordination requirement. Cannot switch to alternative networks without abandoning social/economic ties established on the dominant platform. Network effects create permanent subordination: benefits flow to early adopters; latecomers pay switching costs and liquidity disadvantages. Maximum extraction with minimal coordination benefit from latecomers' position.
constraint_indexing:constraint_classification(network_cold_start_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING NETWORK PROJECT (TANGLED ROPE) — Faces genuine coordination problem: building critical mass requires simultaneous recruitment of many participants. But also faces asymmetric extraction: dominant network siphons liquidity and attention through network effects lock-in. Both coordination function (how do you bootstrap network participation?) and extraction mechanism (dominant network's incumbency advantage) present.
constraint_indexing:constraint_classification(network_cold_start_problem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Primary beneficiary. Experiences network cold start as a pure coordination problem to solve: how to achieve sufficient scale to create value for all participants. Once critical mass is reached, the operator arbitrages liquidity and network effects. Entry and exit are unconstrained for the operator — if this network fails, launch another. Effective extraction flows toward the operator from network effects and switching cost dynamics.
constraint_indexing:constraint_classification(network_cold_start_problem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY INTERVENTION COALITION (SCAFFOLD) — Organized coalition (interoperability advocates, competition authorities, open standards bodies) sees network lock-in as a temporary problem solvable through regulatory intervention: data portability mandates, interoperability requirements, open APIs. These measures have sunset logic built in: once competing networks can interoperate seamlessly, the switching cost advantage of incumbents degrades. Theater is relatively high (political positioning around 'open internet' narratives), but functional sunset clause exists if interoperability regulations take effect.
constraint_indexing:constraint_classification(network_cold_start_problem, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NETWORK NEUTRALITY PRINCIPLE (PITON) — The original function (prevent discriminatory content blocking) has atrophied as the primary extraction mechanism shifted to network effects and switching costs rather than ISP-level discrimination. Network neutrality remains enforced in law and policy but provides minimal protection against platform-level lock-in. Theater ratio high: extensive regulatory framing of network neutrality in public discourse despite limited functional relevance to current extraction mechanisms.
constraint_indexing:constraint_classification(network_cold_start_problem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION-THEORETIC VIEW (MOUNTAIN) — From a pure information-theoretic perspective, the cold start problem reflects an inherent asymmetry in information distribution: early participants possess knowledge of value distribution before late participants arrive. This informational advantage is irreducible — you cannot bootstrap a network where all participants simultaneously know the value of participation. However, structural data contradicts this mountain classification. The extraction mechanisms (network effects, switching costs, liquidity advantages) are institutional, not natural-law. The engine will flag this as a false summit — naturalizing contingent design choices as inherent limits.
constraint_indexing:constraint_classification(network_cold_start_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_cold_start_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_cold_start_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_cold_start_problem, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_cold_start_problem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(network_cold_start_problem, TR),
    TR >= 0.70.

:- end_tests(network_cold_start_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. The cold start problem creates genuine coordination value early (baseline 0.15) — early participants solve the real problem of scaling from zero. But as the network nears and passes critical mass, extractiveness rises sharply (0.52 by endpoint) because the primary value capture mechanism shifts from coordination (recruiting participants) to lock-in (retaining them despite degraded alternatives). The rise reflects not a change in the underlying coordination function but crystallization of switching cost advantage. Suppression (0.58): Moderate-high. Latecomers face high costs to exit: data portability is limited, APIs constrain third-party access, switching incurs social cost (abandoning accumulated social graph), and alternative networks lack liquidity. But suppression is not total — regulatory interventions are lowering barriers (GDPR data portability, upcoming interoperability mandates). Theater ratio (0.61): Moderate-high. Significant performative content surrounds network cold start discourse: 'network effects are natural,' 'scale is inevitable for startups,' 'winner-take-all is the nature of networks.' These framings naturalize what are actually contingent institutional choices (recommendation algorithms, payment system gatekeeping, API restrictions). However, some performative content is justified — the cold start coordination problem is real; the challenge is distinguishing genuine coordination necessity from extraction mechanism disguised as coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The platform operator sees pure coordination (rope) — they are solving the legitimate cold start problem. Early adopters see favorable coordination (rope with low personal extraction) — they are capturing value from scale-up. Latecomers see extraction disguised as coordination (snare) — the cold start problem was real but is now solved; they face lock-in costs for network effects that benefit incumbents, not themselves. Competing networks see hybrid coordination-extraction (tangled rope) — they face genuine bootstrap challenges but exacerbated by incumbent network effects. The regulatory coalition sees a temporary problem solvable through mandated interoperability (scaffold) — sunset logic is empirical if regulatory interventions succeed. The network neutrality principle persists as degraded ritual (piton) — it prevents ISP discrimination but does nothing about platform-level lock-in, the actual extraction mechanism. The civilizational analytical observer risks a false summit (mountain) — naturalizing network effects as immutable law when they are contingent institutional choices. The gap between early-adoption rope perspective and latecomer snare perspective reveals how the same constraint transitions from coordination to extraction as the market matures.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from structural position. The platform operator with arbitrage options experiences low d (full beneficiary, high exit freedom) — they solve the coordination problem and capture network effects. Early adopters with mobile exit options experience moderate d (beneficiary but constrained by eventual network saturation) — they enjoy early advantage but face declining returns as market matures. Latecomer participants with trapped exit experience high d (full target, no exit without losing accumulated social value) — they bear switching costs and liquidity disadvantages. Competing network projects with constrained exit experience high d (target of network effects headwind) — they face structural disadvantage in recruiting critical mass. The regulatory coalition with constrained but organized exit experiences moderate d — they can advocate policy change but cannot individually exit the dominant network. The analytical observer with analytical exit experiences the highest d measurement asymmetry — they can see the full structure but their position doesn't change the extraction dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: The cold start problem resolves mandatrophy by showing how a constraint can start as pure coordination (rope) and transition to hybrid coordination-extraction (tangled rope) to extraction-dominant (snare) as the lifecycle unfolds. At t=0, the platform operator faces a genuine coordination problem: how to bootstrap without critical mass? This is rope. At t=4, early adopters have solved the problem and are now extracting value from latecomers through network effects and switching costs. This is tangled rope — genuine coordination benefit (the network exists and has utility) overlaid with asymmetric extraction (early movers captured disproportionate value). At t=6, for latecomers, the coordination function has fully crystallized into extraction mechanism — the network exists, they must use it, they cannot exit. This is snare. The mandatrophy is not 'which type is correct?' but 'at what point does this transition from coordination to extraction, and who experiences that transition?' The analytical observer who treats network effects as a natural law (mountain) is naturalizing what is actually a temporal transition from rope to snare. The regulatory observer who sees interoperability mandates as a sunset (scaffold) is betting that technological change can reverse the lock-in trajectory. Both perspectives are meaningful but neither fully captures the structural reality — the constraint genuinely transitions types as it matures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_reversibility,
    'Once a network achieves critical mass and dominance, are the network effects irreversible, or does technological or regulatory change enable genuine switching at scale?',
    'Historical case studies: MySpace to Facebook transition, WeChat dominance in China despite regulatory pressure, Discord''s challenge to Slack. Measurement of switching costs as regulatory barriers decline (data portability, APIs, interoperability mandates).',
    'If irreversible: snare classification justified; latecomers permanently trapped. If reversible: scaffold classification justified; sunset logic is empirical; latecomer extraction is temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_reversibility, empirical, 'Whether network tipping points are reversible or permanent').

omega_variable(
    cold_start_bootstrap_mechanism_sufficiency,
    'Does the platform operator''s investment in artificial seeding (subsidized early users, fake liquidity, promotional incentives) constitute genuine coordination problem-solving or extractive market manipulation?',
    'Comparative analysis of platforms with heavy seeding vs. organic growth. Measurement of whether seeded participants provide sustainable value or merely inflate metrics during the critical early phase.',
    'If genuine problem-solving: platform operator''s perspective is rope (coordination). If manipulation: platform operator''s perspective is snare (extraction disguised as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cold_start_bootstrap_mechanism_sufficiency, empirical, 'Whether artificial seeding is coordination or manipulation').

omega_variable(
    interoperability_effectiveness_threshold,
    'At what level of interoperability (APIs, data portability, message protocol compatibility) do switching costs actually decline to the point that latecomers face genuinely unconstrained exit?',
    'Measurement of user switching rates as interoperability regulations take effect; correlation between interoperability scope and exit option improvement for trapped populations.',
    'If interoperability insufficient: scaffold sunset clause is aspirational; latecomer extraction persists. If interoperability sufficient: regulatory scaffold is structural; network lock-in is solvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_effectiveness_threshold, empirical, 'Minimum interoperability threshold for effective exit').

omega_variable(
    network_value_authenticity,
    'Is the value generated by network effects genuine (participants use the network because others do, creating authentic coordinated activity) or artificially inflated (engagement metrics driven by algorithmic manipulation, bot activity, or manufactured FOMO)?',
    'Audit of genuine user activity vs. algorithmic amplification; measurement of authentic coordination benefits vs. platform-generated artificial scarcity.',
    'If genuine: network effects are natural law; cold start is an inherent coordination problem. If artificial: network effects are constructed; cold start extraction is entirely institutional and solvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_value_authenticity, empirical, 'Whether network value is genuine or artificially inflated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_cold_start_problem, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncs_tr_t0, network_cold_start_problem, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ncs_tr_t2, network_cold_start_problem, theater_ratio, 2, 0.44).
narrative_ontology:measurement(ncs_tr_t4, network_cold_start_problem, theater_ratio, 4, 0.54).
narrative_ontology:measurement(ncs_tr_t6, network_cold_start_problem, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(ncs_be_t0, network_cold_start_problem, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ncs_be_t2, network_cold_start_problem, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(ncs_be_t4, network_cold_start_problem, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(ncs_be_t6, network_cold_start_problem, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_cold_start_problem, resource_allocation).
narrative_ontology:affects_constraint(network_cold_start_problem, regulatory_capture_platform_markets).
narrative_ontology:affects_constraint(network_cold_start_problem, switching_cost_lock_in).
narrative_ontology:affects_constraint(network_cold_start_problem, algorithmic_ranking_extraction).

% DUAL FORMULATION NOTE:
% The network cold start problem decomposes into two structurally distinct claims: (1) the bootstrap coordination problem (how to reach critical mass from zero), which has ε ≈ 0.15 and classifies as rope; (2) the lock-in extraction problem (how network effects trap latecomers), which has ε ≈ 0.52 and classifies as snare. These are causally linked (bootstrap success enables lock-in) but empirically distinct. The measured constraint (ε=0.52) reflects the mature network state where lock-in dominates. See network.affects_constraints entries for upstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_cold_start_problem, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
