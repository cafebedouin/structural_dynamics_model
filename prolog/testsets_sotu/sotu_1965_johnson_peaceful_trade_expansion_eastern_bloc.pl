% ============================================================================
% CONSTRAINT STORY: sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, []).

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
 *   constraint_id: sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc
 *   human_readable: Peaceful Trade Expansion with Eastern Bloc (1965 Johnson Administration)
 *   domain: geopolitical/economic_policy/cold_war
 *
 * SUMMARY:
 *   The 1965 Johnson Administration's proposal to expand peaceful trade with
 *   the Soviet Union and Eastern European satellite nations represents a
 *   structural shift in Cold War policy: from pure containment and embargo to
 *   dual-track engagement combining military deterrence with economic
 *   confidence-building. The constraint exhibits hybrid character — it solves
 *   genuine coordination problems (how to engage the Soviet sphere without
 *   military escalation, how to provide Eastern European economies with
 *   autonomy alternatives) while simultaneously extracting from Cold War
 *   hardliners (who lose embargo leverage) and creating asymmetric
 *   dependencies (Eastern European nations gain market access but become
 *   dependent on U.S. commercial relationships). The theater ratio (0.58)
 *   reflects that much of the rhetorical commitment to peaceful trade as a
 *   confidence-building mechanism persists even as enforcement mechanisms
 *   degrade — embargo regimes collapse but parallel military deterrence
 *   remains the true enforcement architecture. The extractiveness trajectory
 *   (0.35→0.52 over 15 years) shows accumulating extraction as domestic
 *   opposition crumbles and the policy becomes normalized, while the theater
 *   ratio rises as the rhetoric of peaceful engagement persists despite
 *   implementation challenges.
 *
 * KEY AGENTS:
 *   - U.S. Exporters: Primary institutional beneficiary (institutional/arbitrage) — gain market access and first-mover advantage in Eastern markets
 *   - Eastern European Economies: Organized actors (organized/constrained) — gain autonomy alternatives but develop dependency on Western trade; constrained exit because reverting to Soviet monopoly becomes costlier
 *   - Diplomatic Engagement Actors: Institutional beneficiaries (institutional/arbitrage) — gain non-military engagement channels; solve the coordination problem of Cold War communication
 *   - Cold War Hardliners: Primary victims (powerless/trapped) — lose embargo leverage and ideological clarity; cannot exit the emerging bipartisan consensus without rejecting Cold War framework entirely
 *   - Protected Domestic Industries: Moderate victims (moderate/constrained) — face direct competition; can exit via lobbying/relocation but face high costs; suppression is structural (tariffs become politically unsustainable)
 *   - Cold War Military Deterrence Architecture: Institutional persistence (institutional/arbitrage) — continues as enforcement mechanism but becomes partially theatrical; embargo regimes degrade while weapons capability persists
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent Cold War policy innovation as immutable law of great power relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, 0.52).
domain_priors:suppression_score(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, 0.65).
domain_priors:theater_ratio(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, tangled_rope).
narrative_ontology:human_readable(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, "Peaceful Trade Expansion with Eastern Bloc (1965 Johnson Administration)").
narrative_ontology:topic_domain(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, "geopolitical/economic_policy/cold_war").

domain_priors:requires_active_enforcement(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, us_exporters).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, eastern_european_economies).
narrative_ontology:constraint_beneficiary(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, diplomatic_engagement_actors).
narrative_ontology:constraint_victim(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, cold_war_hardliners).
narrative_ontology:constraint_victim(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, protected_domestic_industries).
narrative_ontology:constraint_victim(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, soviet_union_monopoly_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLD WAR HARDLINERS (SNARE) — Trapped by the emerging bipartisan consensus that peaceful trade expansion is inevitable policy. Cannot exit the constraint without rejecting Cold War framing entirely. Bear the cost of erosion of embargo leverage and ideological clarity. Maximum extraction from this group's perspective — the institutional framework removes their veto power over Eastern Bloc engagement without their consent.
constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROTECTED DOMESTIC INDUSTRIES (SNARE) — Steel, coal, agriculture face direct competition from Eastern European exports. Suppression is structural: tariffs and quotas become politically unsustainable as trade expansion becomes official policy. High exit costs (lobbying, relocation, consolidation) but exit is possible. Classification reflects significant extraction with constrained rather than trapped exit.
constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. EXPORTERS (ROPE) — Primary beneficiaries of expanded market access. Coordination mechanism enables participation in new markets; suppression of competitors (domestic protectionists) benefits this group. Experience the constraint as pure coordination — the framework solves the collective action problem of opening markets while managing domestic political risk. Net beneficiary with full arbitrage options.
constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EASTERN EUROPEAN ECONOMIES (TANGLED ROPE) — Organized actors seeking alternatives to Soviet economic control. Trade expansion provides genuine coordination benefit (market access, technology transfer, economic autonomy). But the constraint also locks them into asymmetric dependency on U.S. market access — they cannot exit without losing the negotiating leverage they've gained. Hybrid: coordination function (trade enables autonomy) + extraction (dependency relationship).
constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: DIPLOMATIC ENGAGEMENT ACTORS (ROPE) — State Department, international relations agencies, presidential advisory networks. Trade expansion solves the coordination problem of how to engage Soviet sphere without military escalation. This group experiences the constraint as pure coordination — it enables communication channels, builds confidence, and creates non-zero-sum engagement mechanisms. Beneficiaries with arbitrage options (can shift engagement models).
constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR MILITARY DETERRENCE ARCHITECTURE (PITON) — The parallel enforcement mechanism (NATO, strategic weapons, intelligence apparatus) persists while trade expansion proceeds. Theater ratio is high (0.58) because much of the 'deterrence' is performative — the credible threat comes from weapons stockpiles and force posture, not from embargo regimes. Trade expansion does not eliminate military deterrence but reveals it as partially theatrical. Piton classification reflects degraded function of embargo as deterrent while the institutional structure persists.
constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALPOLITIK VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears as an immutable feature of interstate relations: great powers always use trade as a confidence-building mechanism parallel to military deterrence. This view naturalizes the policy as an inevitable law of international relations. However, the structural data (identifiable beneficiaries, suppression of alternative regimes, active enforcement requirements) contradicts the mountain classification — this is a constructed institutional arrangement naturalized as realpolitik law.
constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, TR),
    TR >= 0.70.

:- end_tests(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint solves coordination problems for exporters, diplomats, and Eastern European economies seeking alternatives to Soviet monopoly. But it simultaneously suppresses Cold War hardliners' preferred policy regime (embargo) and creates asymmetric dependencies. The trajectory from 0.35 to 0.52 reflects that initial extraction is modest (new policy, limited implementation) but accelerates as domestic opposition crumbles and the policy becomes normalized — hardliners have less recourse to block further expansion. Suppression (0.65): Moderate-high. Structural barriers include Cold War consensus (reverting requires delegitimizing the entire containment framework), technology control regimes (export control boards), and Congressional oversight. But suppression is not total — protected industries can lobby for exemptions, hardliners can publicize risks, and Soviet cooperation can be withdrawn. Theater ratio (0.58): Moderate. Peaceful trade expansion is partly genuine coordination (solves communication problem, enables market access) and partly performative (rhetoric of confidence-building masks continued military deterrence, selective embargo persistence, technology control). The theater increases over time (0.42→0.58) as the policy rhetoric becomes decoupled from implementation — rhetoric emphasizes peaceful engagement but enforcement remains selective and military deterrence remains the credible commitment.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim is extreme. Exporters and diplomats see coordination enabling; hardliners and protected industries see regime change extracting. The gap reflects that peaceful trade expansion is not a neutral policy innovation but a structural redistribution: it transfers veto power from hardliners to exporters, transfers market protection from domestic industries to foreign suppliers, and transfers engagement leverage from military deterrence specialists to trade negotiators. Each perspective correctly identifies the constraint's effect on their position — the gap is not perceptual but structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. U.S. exporters (beneficiary + arbitrage exit) derive d≈0.10, experiencing minimal extraction. Cold War hardliners (victim + trapped exit) derive d≈0.95, experiencing maximal extraction. Eastern European economies (mixed beneficiary/victim + constrained exit) derive d≈0.55, experiencing moderate extraction offset by autonomy benefits. Protected industries (victim + constrained exit) derive d≈0.75, experiencing significant extraction but with possible exit paths (higher costs than beneficiaries). The Tangled Rope classification emerges from this mixed directionality profile combined with genuine coordination (trade enables market access and autonomy) and asymmetric extraction (hardliners lose leverage, dependencies form).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy — classified as Tangled Rope with extractiveness 0.52, below the 0.70 threshold that requires mandatrophy resolution. However, the perspectival gap (Snare from hardliner perspective, Rope from exporter perspective) demonstrates the core mandatrophy tension: is this coordination solving a genuine collective action problem (how to engage Soviet sphere), or is it pure extraction disguised as coordination? The tension resolves through temporal analysis: at t=0, the policy appears more coordinative (genuine market-opening benefit, communication channels). By t=15, the extractive character becomes clearer (dependencies solidify, hardliner veto is permanently eroded, theater increases). The trajectory suggests the constraint is shifting toward higher extractiveness and higher theater — early genuine coordination followed by institutional capture and normalization. A 20-year analysis might show crossing the 0.70 threshold, at which point mandatrophy resolution becomes required. Current status: Tangled Rope with rising extractiveness trajectory — monitor for mandatrophy emergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sino_soviet_split_leverage,
    'Does peaceful trade expansion with Eastern Europe exploit the Sino-Soviet split to fragment Soviet bloc cohesion, or does it stabilize Soviet control by offering safety valve economics?',
    'Historical trajectory analysis: do Eastern European economies that gain trade access show increased autonomy from Moscow or increased integration with Soviet planning? Do satellite nations use expanded Western trade to negotiate greater independence or to supplement Soviet integration?',
    'If fragmentation: trade expansion is a geopolitical weapon disguised as coordination (Snare from Soviet perspective). If stabilization: trade expansion enables Soviet sphere to persist longer (Rope from Soviet perspective). Classification impacts whether this is destabilizing confidence-building or stabilizing co-optation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sino_soviet_split_leverage, empirical, 'Whether trade expansion fragments or stabilizes Soviet bloc cohesion').

omega_variable(
    technology_transfer_asymmetry,
    'Do expanded trade channels enable Soviet technology acquisition that would strengthen Soviet military capacity, or does U.S. export control (selective expansion) prevent militarily significant transfer?',
    'Declassified NSC assessments of Cold War technology transfer risks; historical analysis of Soviet acquisition patterns and whether peaceful trade expansion accelerated or delayed military-relevant technology access',
    'If significant transfer: trade expansion is extraction of U.S. security advantage (Snare from U.S. national security perspective). If controlled: trade expansion is managed confidence-building with security guardrails (Rope from diplomatic perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_asymmetry, empirical, 'Technology transfer asymmetry in trade expansion channels').

omega_variable(
    domestic_political_coalition_stability,
    'Is the bipartisan consensus for peaceful trade expansion stable enough to survive a Cold War crisis, or is it theater that collapses under geopolitical stress?',
    'Longitudinal analysis of policy continuity across administrations; assessment of how quickly consensus reversed during specific Cold War incidents (Berlin Wall, Cuban Missile Crisis aftermath, Vietnam escalation). Does peaceful trade expansion persist or revert to embargo logic when crisis pressure increases?',
    'If stable: trade expansion is a genuine structural shift in Cold War engagement (Tangled Rope). If fragile: it is a performative policy that reverts under pressure (Piton or Snare depending on agent perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_political_coalition_stability, empirical, 'Stability of bipartisan consensus on peaceful trade expansion').

omega_variable(
    eastern_european_autonomy_trajectory,
    'Do Eastern European nations that gain trade access use it to build genuine autonomy from Soviet control, or does the Soviet Union co-opt trade channels to deepen satellite dependency?',
    'Comparative analysis of Eastern European countries with varying degrees of Western trade access; measurement of economic indicators (GDP growth, technology adoption, workforce emigration), political indicators (reform movements, leadership autonomy), and dependency measures (trade concentration, debt servicing). Do nations with more trade access show greater or lesser autonomy from Moscow?',
    'If autonomy: trade expansion is a genuine confidence-building mechanism enabling satellite liberation (Rope/Scaffold from Eastern European perspective). If co-opted: trade expansion is a mechanism of Soviet control, extractive for satellite nations (Snare from their perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eastern_european_autonomy_trajectory, empirical, 'Whether trade expansion enables or constrains Eastern European autonomy').

omega_variable(
    enforcement_mechanism_credibility,
    'What enforcement mechanism prevents Cold War hardliners and Soviet sphere interests from undermining peaceful trade expansion? Is enforcement institutional (consensus, legal framework) or structural (dependency making reversal costly)?',
    'Analysis of veto points: what institutional actors could reverse the policy and at what political cost? Identification of feedback loops that make sustained embargo regime costlier than trade expansion. Assessment of whether enforcement depends on continued U.S.-Soviet cooperation or can persist despite cooperation breakdown.',
    'If institutional enforcement: policy depends on maintained consensus (fragile). If structural enforcement: policy is self-sustaining through economic dependency (stable). Classification of Tangled Rope vs. Piton depends partly on enforcement durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_credibility, empirical, 'Enforcement mechanism and durability of peaceful trade expansion policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peacetrade_theater_t0, sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, theater_ratio, 0, 0.42).
narrative_ontology:measurement(peacetrade_theater_t5, sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, theater_ratio, 5, 0.5).
narrative_ontology:measurement(peacetrade_theater_t10, sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, theater_ratio, 10, 0.58).
narrative_ontology:measurement(peacetrade_theater_t15, sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(peacetrade_extractiveness_t0, sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(peacetrade_extractiveness_t5, sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(peacetrade_extractiveness_t10, sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(peacetrade_extractiveness_t15, sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, resource_allocation).
narrative_ontology:affects_constraint(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, soviet_military_technological_advancement).
narrative_ontology:affects_constraint(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, eastern_european_national_autonomy).
narrative_ontology:affects_constraint(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, us_export_dependent_industry_formation).
narrative_ontology:affects_constraint(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, cold_war_consensus_stability).

% DUAL FORMULATION NOTE:
% Peaceful trade expansion decomposed into: (1) market access coordination (upstream, Rope-dominant), (2) Cold War policy regime shift (downstream, Snare-dominant from hardliner perspective, Tangled Rope from systemic perspective). Each has different ε and different beneficiary/victim profiles. They are linked because the policy innovation is downstream of market access problems in export industries but flows upstream to constraint Cold War consensus itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
