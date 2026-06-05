% ============================================================================
% CONSTRAINT STORY: trump_making_china_great_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_making_china_great_2026, []).

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
 *   constraint_id: trump_making_china_great_2026
 *   human_readable: The Trumpian Post-Western Order
 *   domain: political/economic/geopolitical
 *
 * SUMMARY:
 *   The Trump return in 2024-2026 has accelerated a shift from the US-led
 *   liberal international order (established post-1945, institutionalized
 *   post-1991) toward a multipolar post-Western arrangement. This is not
 *   simply geopolitical rebalancing — it is a constraint structure that
 *   extracts from certain groups (developing states, EU political autonomy,
 *   global supply-chain stability) while offering coordination benefits to
 *   others (regional powers, nationalist elites, corporations able to
 *   arbitrage fragmentation). The constraint exhibits the full range of
 *   Deferential Realism types depending on the observer's structural
 *   position: small developing states experience it as a Snare (no exit); the
 *   EU experiences Tangled Rope (forced geopolitical realignment extracts
 *   autonomy while NATO coordination persists); the US and Chinese
 *   establishments experience Rope (constraint functions as coordination
 *   mechanism for their respective projects); multinational corporations see
 *   Tangled Rope (supply-chain arbitrage benefits offset by fragmentation
 *   costs); liberal institutions appear as Piton (performative rules without
 *   enforcement); civilizational observers risk seeing Mountain (treating
 *   multipolar transition as inevitable law of hegemonic succession rather
 *   than contingent policy choice). The theater ratio (0.68) reflects that
 *   the post-Western order maintains rhetorical commitment to liberal
 *   principles while dismantling liberal institutions — Trump simultaneously
 *   claims to defend democracy while undermining multilateral norms; China
 *   claims regional cooperation while executing debt-trap Belt & Road; EU
 *   claims strategic autonomy while deepening dependency on US security. The
 *   measurement trajectory shows extractiveness rising from 0.38 to 0.58 over
 *   four years as initial trade war escalation, institutional withdrawal, and
 *   supply-chain fragmentation compound into structural dependency.
 *
 * KEY AGENTS:
 *   - Donald Trump Administration (US): Nationalist elites (powerful/arbitrage) — benefits from reshoring, rearmament, reduced multilateral obligations; enforces constraint via unilateral tariffs, alliance pressure, sanctions
 *   - Chinese Communist Party: Regional hegemon (institutional/arbitrage) — benefits from US withdrawal from Asia, accelerates BRI and RCEP integration; enforces constraint via debt, market access, military presence
 *   - EU Political Leadership: Trapped institutional actor (organized/constrained) — faces forced choice between US security guarantee and Chinese trade dependency; constrained by NATO obligation and supply-chain reality
 *   - Developing States (Vietnam, Indonesia, Kenya, Nigeria): Primary victims (powerless/trapped) — no genuine third path; forced into binary choice between US-aligned and China-aligned, with extraction mechanisms built into both options
 *   - Multinational Corporations: Adaptive intermediary (powerful/mobile) — arbitrage opportunities in fragmented supply chains, but face forced capital reallocation and tariff barriers; moderately constrained but with exit capacity
 *   - Liberal International Institutions (UN, WTO, IMF): Degraded coordinators (institutional/arbitrage) — their enforcement authority eroded; persist through inertia but no longer meaningfully constrain great powers
 *   - Global Supply Chain System: Abstract victim (powerless/trapped) — subjected to reshoring mandates, tariff barriers, de-risking requirements; efficiency losses mount while coordination benefits decline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_making_china_great_2026, 0.58).
domain_priors:suppression_score(trump_making_china_great_2026, 0.72).
domain_priors:theater_ratio(trump_making_china_great_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_making_china_great_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(trump_making_china_great_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trump_making_china_great_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_making_china_great_2026, tangled_rope).
narrative_ontology:human_readable(trump_making_china_great_2026, "The Trumpian Post-Western Order").
narrative_ontology:topic_domain(trump_making_china_great_2026, "political/economic/geopolitical").

domain_priors:requires_active_enforcement(trump_making_china_great_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_making_china_great_2026, regional_powers_china_russia_india).
narrative_ontology:constraint_beneficiary(trump_making_china_great_2026, nationalist_elites_us_europe).
narrative_ontology:constraint_victim(trump_making_china_great_2026, liberal_international_order_institutions).
narrative_ontology:constraint_victim(trump_making_china_great_2026, global_supply_chain_stability).
narrative_ontology:constraint_victim(trump_making_china_great_2026, middle_income_developing_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING STATE (SNARE) — Small-to-medium economies (Vietnam, Indonesia, Kenya, Mexico) face binary choice: enter Chinese BRI dependency or US security alliance, with no genuine third path. Both offer extraction disguised as development. Trapped by capital flight risk, debt servicing requirements, and security dependence. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.68.
constraint_indexing:constraint_classification(trump_making_china_great_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EUROPEAN UNION (TANGLED ROPE) — Coordination function (NATO, trade bloc) but asymmetric extraction: must choose between US security guarantee and China trade dependency, loses strategic autonomy either way. Enforcement via tariffs, sanctions, currency manipulation. d≈0.68, f(d)≈1.05, σ=1.1 → χ≈0.53.
constraint_indexing:constraint_classification(trump_making_china_great_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US CORPORATE-MILITARY COMPLEX (ROPE) — Benefits from multipolar instability: reshoring, defense budgets, de-risking supply chains from China create US industrial reinvestment opportunities. Sees constraint as coordination mechanism for rebuilding US capacity. d≈0.15, f(d)≈0.05, σ=1.2 → χ≈0.04.
constraint_indexing:constraint_classification(trump_making_china_great_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINESE COMMUNIST PARTY (ROPE) — Decoupling and deglobalization accelerate China's intra-Asia integration (RCEP, Belt & Road expansion, yuan settlement). Constraint functions as coordination mechanism for Chinese regional hegemony. d≈0.12, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(trump_making_china_great_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTINATIONAL CORPORATIONS (TANGLED ROPE) — Coordination benefit (two competing blocs create arbitrage opportunities, lower taxes, regulatory shopping). Extraction cost (forced supply-chain fragmentation, tariffs, de-risking mandates destroy economies of scale, require massive capital reallocation). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(trump_making_china_great_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LIBERAL INTERNATIONAL INSTITUTIONS (PITON) — Once functional coordination (trade rules, dispute settlement, development norms), now largely performative. Enforcement authority eroded by US unilateralism, Chinese non-compliance, rising nationalist sovereigntism. Persist through bureaucratic inertia and lack of alternatives, but their actual power to coordinate or constrain is diminished. theater_ratio=0.68 (rules exist but widely violated). d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(trump_making_china_great_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN RISK) — May naturalize the multipolar transition as inevitable consequence of hegemonic decline and rising great powers, treating it as structural law of international relations (Thucydides trap, decline theory). But the structural data (ε=0.58, suppression=0.72, theater=0.68) contradicts mountain criteria — the post-Western order is contingent on policy choices (tariffs, trade war escalation, sanctions regimes), not intrinsic to geopolitics. Engine will flag false summit.
constraint_indexing:constraint_classification(trump_making_china_great_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_making_china_great_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_making_china_great_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_making_china_great_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_making_china_great_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trump_making_china_great_2026, TR),
    TR >= 0.70.

:- end_tests(trump_making_china_great_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The post-Western order extracts from multiple groups but not equally. Developing states face debt dependency and security coercion — core extraction mechanisms. The EU loses strategic autonomy through forced alignment — structural extraction. Global supply-chain stability suffers through fragmentation mandates — distributed extraction. But the extraction is not total (0.80+) because some actors (US corporate, Chinese state) have genuine coordination benefits, and middle powers retain some negotiating capacity. Suppression (0.72): High. Alternative pathways are actively closed: US sanctions regime suppresses third-country neutrality; Chinese market access is conditional on geopolitical alignment; EU is pressured by both poles to choose; multilateral institutions are undermined rather than reformed. Suppression is not absolute because some middle-power coordination attempts (BRICS, ASEAN, AU) persist, though under significant pressure. Theater ratio (0.68): High-moderate. Significant performative content: both US and China claim to defend liberal values while dismantling liberal institutions; trade war justifications invoke national security while targeting economic competitors; sanctions regimes are labeled 'values-based' while following geopolitical logic; institutions continue issuing reports and convening meetings while having lost enforcement authority. The theater reflects that elites maintain rhetorical commitment to the order they are dismantling.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits acute perspectival divergence. The US establishment sees Rope (constraint as coordination mechanism for American reshoring, military dominance, alliance restructuring). The Chinese state sees Rope (constraint as mechanism for regional hegemony and yuan settlement expansion). The EU sees Tangled Rope (forced geopolitical realignment, loss of strategic autonomy, but NATO coordination persists). Developing states see Snare (binary choice with extraction mechanisms in both branches). Multinational corporations see Tangled Rope (arbitrage opportunities offset by fragmentation costs). Liberal institutions see Piton (performative rule-following, diminished enforcement, persistence through inertia). The analytical observer risks seeing Mountain (treating multipolarity as inevitable law of hegemonic transition) — but the structural data reveals this as false summit: the post-Western order is contingent on policy choices (tariff escalation, alliance restructuring, institutional withdrawal), not structural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing States: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction exposure. Regional Powers (China, Russia, India): Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary; constraint functions as their coordination mechanism. US Corporate-Military: Beneficiary + arbitrage → d≈0.15, f(d)≈0.05. Net beneficiary; reshoring and rearmament create profit opportunities. EU Political Leadership: Victim + constrained → d≈0.68, f(d)≈1.05. High extraction (forced geopolitical realignment); constrained exit (NATO obligations, security dependency). Multinational Corporations: Mixed + mobile → d≈0.50, f(d)≈0.65. Symmetric exposure (fragmentation costs offset by arbitrage benefits); mobile exit (can relocate production). Liberal Institutions: Institutional + arbitrage → d≈0.05, f(d)≈-0.09. Piton classification from theater gate, not directionality. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Risk of naturalizing contingent arrangement as law.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the post-Western order is a Tangled Rope at the system level (ε=0.58, χ≈0.53-0.58 depending on agent), not a pure Snare or pure Rope. It combines: (1) genuine coordination functions — multipolar competition can incentivize efficiency, innovation, regional integration, and military deterrence stability; (2) asymmetric extraction — benefits flow concentrated to regional powers and nationalist elites, costs distributed across developing states and global commons. The key insight: the constraint is only Rope from the beneficiary perspectives (US, China). It is Snare from the developing-state perspective. It is Tangled Rope from the moderate-power perspective (EU, MNCs). These are not competing interpretations of a single truth — they are structural facts about who benefits and who bears costs. The false mountain risk is that civilizational analysts may naturalize this arrangement as inevitable geopolitical law, obscuring that it results from Trump's policy choices (withdrawal from TPP, Paris Agreement rejection, NATO pressure, tariff escalation) and is reversible if different choices are made. The mandatrophy is resolved by showing that the extraction is real (not mere coordination cost) AND the coordination is real (not mere cover for extraction) — both are structurally present, which is precisely the definition of Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liberal_order_salvageability,
    'Is the liberal international order fundamentally broken by Trump-era unilateralism and Chinese rise, or can reformed institutions (expanded WTO, climate club, selective multilateralism) preserve coordination on specific domains?',
    'Empirical tracking of institutional performance (trade dispute resolution speed, climate agreement compliance, development finance flow rates) under post-Western conditions; identification of which coordination functions survived and which collapsed.',
    'If salvageable: constraint is Tangled Rope with high-extraction overlay (coordination core + nationalist rent-seeking). If irreversible: constraint is Snare with false mountain framing (naive analysts see structural inevitability; structural reality is choice to decompose institutions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_order_salvageability, empirical, 'Whether liberal international institutions can be reformed to function in post-Western environment').

omega_variable(
    multipolarity_extraction_asymmetry,
    'In a post-Western multipolar order, who extracts more: US reshoring/military dominance, or Chinese regional hegemony/BRI debt, or distributed extraction by all regional powers equally?',
    'Comparative benefit analysis: US manufacturing job creation vs. income stagnation; Chinese BRI debt-to-GDP ratios and repayment defaults; regional power capacity to impose terms on smaller states. Index extraction by beneficiary.',
    'If extraction is symmetric: constraint is Rope (multipolarity as balanced coordination). If asymmetric to China: constraint is Snare with Chinese beneficiary (hegemony via debt/infrastructure). If asymmetric to US: constraint is Tangled Rope with US institutional capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multipolarity_extraction_asymmetry, empirical, 'Distribution of extraction benefits across poles in post-Western order').

omega_variable(
    middle_power_coalition_formation,
    'Can middle powers (India, Indonesia, Brazil, Nigeria, South Africa, Turkey) form genuine alternative coalition that breaks binary US-China choice, or does multipolarity simply expand the number of poles while maintaining traplike extraction?',
    'Tracking coalition formation (BRICS expansion, ASEAN enlargement, African Union institutions, Quad dynamics); measurement of actual decision autonomy (how often middle powers override great-power preferences); analysis of whether alternative coordination mechanisms emerge.',
    'If coalition viable: constraint is Scaffold with sunset (middle power autonomy building, exit path emerging). If not viable: constraint is deep Snare (multipolar appearance masks continued extraction), or multiple Snares (each middle power in individual trap rather than collective cage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middle_power_coalition_formation, empirical, 'Whether middle powers can achieve autonomous coalition or remain trapped in binary poles').

omega_variable(
    deglobalization_coordination_cost,
    'What are the true coordination costs of supply-chain fragmentation, dual currency systems, and regionalized standards? Is this a temporary Scaffold with payoff at the end (resilience), or permanent Tangled Rope extraction (Balkanization drag)?',
    'Measurement of economic efficiency loss from fragmentation; comparison of resilience gains vs. productivity losses; tracking whether standards convergence emerges or diverges further; cost-benefit analysis of dual-system maintenance.',
    'If costs justified: constraint appears as Scaffold (temporary fragmentation with long-term resilience gain). If costs permanent: constraint is Tangled Rope with high extraction (fragmentation as rent-seeking by different elites).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deglobalization_coordination_cost, empirical, 'Whether supply-chain fragmentation costs are temporary (scaffold) or permanent (tangled extraction)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_making_china_great_2026, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tmcg_tr_t0, trump_making_china_great_2026, theater_ratio, 0, 0.52).
narrative_ontology:measurement(tmcg_tr_t2, trump_making_china_great_2026, theater_ratio, 2, 0.6).
narrative_ontology:measurement(tmcg_tr_t4, trump_making_china_great_2026, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(tmcg_be_t0, trump_making_china_great_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tmcg_be_t2, trump_making_china_great_2026, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(tmcg_be_t4, trump_making_china_great_2026, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_making_china_great_2026, global_infrastructure).
narrative_ontology:affects_constraint(trump_making_china_great_2026, us_china_strategic_competition).
narrative_ontology:affects_constraint(trump_making_china_great_2026, dollar_hegemony_erosion).
narrative_ontology:affects_constraint(trump_making_china_great_2026, supply_chain_geopoliticization).
narrative_ontology:affects_constraint(trump_making_china_great_2026, brics_counter_hegemony).

% DUAL FORMULATION NOTE:
% The post-Western order constraint is downstream of Trump's policy choices (tariffs, alliance restructuring, institutional withdrawal) and upstream of several decomposed constraints: the US-China strategic competition (more directly extractive, ε≈0.65+), dollar hegemony erosion (currency-regime constraint, ε≈0.45), supply-chain geopoliticization (sectoral fragmentation, ε≈0.50+), and BRICS counter-hegemony formation (attempted middle-power escape route, ε≈0.35). Each of these has different ε and structural properties; they are linked through this constraint as the system-level envelope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trump_making_china_great_2026, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
