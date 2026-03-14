% ============================================================================
% CONSTRAINT STORY: soviet_embargo_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soviet_embargo_regime, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: soviet_embargo_regime
 *   human_readable: Soviet Embargo Regime (1947-1991)
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The Soviet embargo regime (1947-1991) represents a complex geopolitical
 *   constraint combining genuine collective-action coordination with
 *   substantial asymmetric extraction. Initiated through NATO alliance
 *   coordination to prevent technology transfer to strategic competitor, the
 *   regime exhibits all characteristics of Tangled Rope: it solves a real
 *   coordination problem (preventing Soviet access to critical technologies)
 *   while extracting significantly from Soviet populations and scientific
 *   communities. The constraint demonstrates how the same structural
 *   phenomenon — technology transfer restrictions — appears simultaneously as
 *   coordination mechanism (from Western alliance perspective), extraction
 *   system (from Soviet citizen perspective), autarkic mobilization (from
 *   Soviet state perspective), and increasingly degraded theatrical
 *   enforcement (from late-Cold-War institutional perspective). The theater
 *   ratio increases over the interval (0.40→0.65) reflecting growing
 *   effectiveness of smuggling networks and black-market acquisition,
 *   suggesting that formal embargo mechanisms persist more through
 *   institutional inertia than through actual denial of access by 1980s. This
 *   trajectory supports both the piton classification at civilizational
 *   timescale and the scaffold classification among détente advocates who
 *   perceive negotiated sunset pathways.
 *
 * KEY AGENTS:
 *   - Soviet Citizens and Workers: Primary victims (powerless/trapped) — bear extraction through consumer scarcity, technological lag, reduced living standards; no exit mechanism
 *   - Soviet Scientific/Technical Community: Primary victims (powerless/trapped) — cut off from Western knowledge, duplicating research, isolated from collaboration; structural brain drain and defection losses
 *   - Soviet State Administration: Mixed actor (institutional/constrained) — coordinates autarkic response and uses embargo threat to justify internal consolidation; experiences constraint as external but benefits from enhanced control
 *   - Western Alliance Security Apparatus: Primary beneficiary (institutional/arbitrage) — solves collective action problem of technology denial through coordinated export controls; maximum arbitrage option
 *   - Western Industrial Competitors: Secondary beneficiary (powerful/arbitrage) — gain protected market access, eliminated Soviet competition; choose not to lobby for embargo lifting
 *   - Eastern Bloc Satellite States: Constrained coalition members (organized/constrained) — experience spillover effects and resource extraction for Soviet strategic purposes alongside military protection benefits
 *   - Détente Advocates: Organized reformers (organized/constrained) — perceive negotiation and arms control as sunset mechanisms; represent institutional pressure for de-escalation
 *   - Cold War Bureaucratic Apparatus: Institutional maintainers (institutional/arbitrage) — sustain export controls, licensing regimes, enforcement mechanisms increasingly divorced from actual technology denial
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent embargo policy as inherent to strategic competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soviet_embargo_regime, 0.62).
domain_priors:suppression_score(soviet_embargo_regime, 0.75).
domain_priors:theater_ratio(soviet_embargo_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soviet_embargo_regime, extractiveness, 0.62).
narrative_ontology:constraint_metric(soviet_embargo_regime, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(soviet_embargo_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soviet_embargo_regime, tangled_rope).
narrative_ontology:human_readable(soviet_embargo_regime, "Soviet Embargo Regime (1947-1991)").
narrative_ontology:topic_domain(soviet_embargo_regime, "geopolitical/economic").

domain_priors:requires_active_enforcement(soviet_embargo_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soviet_embargo_regime, western_alliance_security_apparatus).
narrative_ontology:constraint_beneficiary(soviet_embargo_regime, us_industrial_competitors).
narrative_ontology:constraint_beneficiary(soviet_embargo_regime, domestic_strategic_industries).
narrative_ontology:constraint_victim(soviet_embargo_regime, soviet_economy).
narrative_ontology:constraint_victim(soviet_embargo_regime, soviet_technological_development).
narrative_ontology:constraint_victim(soviet_embargo_regime, soviet_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET CONSUMER/WORKER (SNARE) — Structurally trapped by national borders and state control. Bears full extraction cost through consumer goods scarcity, technological lag, and reduced living standards. No exit mechanism. Maximum experienced extraction — powerless agent with zero arbitrage options, trapped within a state that cannot exit the embargo independently.
constraint_indexing:constraint_classification(soviet_embargo_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SOVIET SCIENTIFIC/TECHNICAL COMMUNITY (SNARE) — Trapped by both state control and embargo restrictions. Cut off from Western literature, components, and collaborative opportunities. Forced into duplicative research at technological disadvantage. No exit option — defection carries extreme risk and cost. Pure extraction with no coordination benefit to this agent.
constraint_indexing:constraint_classification(soviet_embargo_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SOVIET STATE ADMINISTRATION (TANGLED ROPE) — Constrained by embargo but also coordinates internal resource allocation and military-industrial production as response to embargo threat. Genuine coordination function (mobilizing autarky, directing innovation effort) coexists with asymmetric extraction from citizens. State sees constraint as external (embargo) but uses it to justify internal consolidation of power. Moderate d-value — benefits from enhanced state control despite economic costs.
constraint_indexing:constraint_classification(soviet_embargo_regime, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: WESTERN ALLIANCE SECURITY APPARATUS (ROPE) — Primary beneficiary with maximum arbitrage. Sees embargo as pure coordination mechanism solving collective action problem: preventing technology transfer to strategic competitor. Net beneficiary — extraction runs from Soviet side toward Western strategic interests. Low experienced chi from this perspective.
constraint_indexing:constraint_classification(soviet_embargo_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WESTERN INDUSTRIAL COMPETITORS (ROPE) — Benefit from protected market access during embargo. Soviet alternative is eliminated, reducing competitive pressure. Beneficiary with arbitrage exit option (could lobby for embargo lifting but choose not to). Net beneficiary — see embargo as pure coordination to mutual advantage.
constraint_indexing:constraint_classification(soviet_embargo_regime, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EASTERN BLOC SATELLITE STATES (TANGLED ROPE) — Constrained by both embargo and Soviet control. Genuine coordination with USSR on countering Western pressure coexists with extraction of resources for Soviet strategic needs. Moderate power through coalition; constrained because they cannot exit the Soviet sphere without severe cost. Mixed experience — some benefit from Soviet military protection, significant cost from embargo spillovers.
constraint_indexing:constraint_classification(soviet_embargo_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: DÉTENTE/ARMS CONTROL ADVOCATES (SCAFFOLD) — Organized actors (diplomats, scholars, some political factions) see embargo as temporary coordination problem solvable through negotiation and arms control frameworks. Sunset mechanism: SALT, Helsinki Accords, cultural exchanges represent partial de-escalation pathways. Suppression declines as trust-building measures implemented. Theater moderate because negotiation itself becomes performative (summits, photo opportunities) but genuine verification mechanisms emerge. Low chi because organized agents perceive agency and exit route.
constraint_indexing:constraint_classification(soviet_embargo_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: COLD WAR INSTITUTIONAL APPARATUS (PITON) — At civilizational timescale, the embargo regime becomes increasingly performative. By 1980s, technology transfer still occurs through black markets, smuggling, and third-party intermediaries. Official embargo controls are maintained through institutional inertia despite degraded function. Theater ratio high (licensing requirements, export control bureaucracies that leak) reflecting that the regime persists more through lack of alternative institutional structures than through effective enforcement. Piton classification derives from theater gate, not high experienced extraction at this perspective.
constraint_indexing:constraint_classification(soviet_embargo_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk perspective that naturalizes the embargo as inherent to bipolar geopolitical structure. 'Strategic adversaries cannot trade' appears as immutable law derivable from realist theory. However, structural data reveals this as false summit: the embargo is contingent on specific institutional choices (CoCom export controls, technology classification regimes, political will to enforce). Historical counterexample: US-China trade and technology relationships despite ongoing strategic competition show that embargo is policy choice, not natural law.
constraint_indexing:constraint_classification(soviet_embargo_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soviet_embargo_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soviet_embargo_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soviet_embargo_regime, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soviet_embargo_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soviet_embargo_regime, TR),
    TR >= 0.70.

:- end_tests(soviet_embargo_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The regime extracts significantly from Soviet-side agents through consumer scarcity, technological isolation, and duplicative innovation costs. However, the extraction is not maximal (snare-level 0.66+) because: (a) Soviet state captures some value through enhanced internal control justification, (b) Eastern Bloc satellite states experience mixed extraction/benefit, (c) black-market and smuggling mechanisms partially defeat suppression. The value increases from 0.50→0.68 through mid-period as embargoed sectors matured and lag accumulated, then declines slightly to 0.62 as enforcement degraded through smuggling. Suppression (0.75): High. Structural barriers to technology access are severe: state control of foreign exchange, travel restrictions, communication barriers, classified research compartmentalization, legal penalties for unauthorized contact with Westerners. However, suppression is not absolute (snare-level 0.85+) because organized smuggling networks, third-country intermediaries, and defecting scientists provided partial workarounds. Theater ratio (0.58): Moderate-high. Early period (0.40) reflects genuine enforcement capability — CoCom export control lists were substantive, Western vigilance was high. Later period (0.65) reflects increasing performativity: smuggling networks defeated formal controls, licensing bureaucracies multiplied while effectiveness declined, endpoint rituals (embargo reviews, compliance certifications) persisted despite known pervasiveness of black-market acquisition.
 *
 * PERSPECTIVAL GAP:
 *   PRIMARY GAP: Western beneficiary (Rope) vs Soviet powerless (Snare). Same embargo mechanism; diametrically opposite classifications. Western agent experiences low suppression and genuine coordination benefit (technology denied to competitor strengthens Western security posture). Soviet citizen experiences maximum suppression and zero coordination benefit (consumer goods denied, scientific isolation enforced, no alternative access pathways). The gap reveals that suppression is raw structural property (unscaled by power/scope) while extractiveness is context-dependent. Measured suppression ~0.75 is shared across both perspectives, but experienced extraction differs maximally because f(d) scales by directionality: d≈0.05 for Western beneficiary produces negative effective chi (they benefit); d≈0.95 for Soviet victim produces maximum positive chi (they bear costs). SECONDARY GAP: Soviet state (Tangled Rope) vs Soviet citizens (Snare). State benefits from embargo justification for internal consolidation; citizens bear extraction. State experiences constraint as external coordination need; citizens experience it as suppression mechanism targeting them. TERTIARY GAP: Formal piton classification (degraded theater by 1980s) vs functional snare classification (suppression remains high throughout). The piton perspective captures that smuggling networks defeated formal enforcement, but the snare remains functional because Soviet state maintains suppression infrastructure regardless of black-market effectiveness — suppression persists through state will rather than through embargo effectiveness, which is exactly what makes it snare-like to the victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural relationship to extraction flow. Soviet citizens are pure targets (d≈0.95): maximum victims, trapped exit, zero arbitrage. Soviet state is mixed (d≈0.50): benefits from coordination frame and internal consolidation, but also bears external costs of embargo. Western security apparatus is full beneficiary (d≈0.05): extraction flows toward them, they have exit via diplomatic negotiation but choose not to use it, arbitrage options maximize their flexibility. Western industrial competitors similarly positioned (d≈0.10): gain protected market, arbitrage via delisting requests they don't exercise. Eastern Bloc satellites constrained between Soviet demands and embargo effects (d≈0.65): moderate extraction, can organize but cannot exit the relationship. Détente advocates have constrained exit but meaningful agency (d≈0.55): organized power, can influence policy trajectory but face institutional resistance. Each agent's d-value positions them in the sigmoid f(d) function to produce their experienced effective extraction chi.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE MANDATROPHY TENSION: Does the embargo regime coordinate technology denial (collective action solution to prevent adversary military modernization) or extract asymmetrically from Soviet populations (leveraging state control to concentrate costs)? The constraint is BOTH: it genuinely solves the Western collective action problem AND it extracts significantly. The mandatrophy resolves through perspectival precision: from Western alliance perspective, the constraint is nearly pure Rope (low suppression needed, genuine coordination benefit). From Soviet citizen perspective, it is nearly pure Snare (maximum suppression, no coordination benefit to this agent). The same ε and χ values produce different classification types depending on (P,T,E,S). The constraint is Tangled Rope precisely because it contains both elements: active enforcement (required for Western security benefit), asymmetric victim group (Soviet powerless agents), and genuine coordination function (technology denial). The false mountain perspective reveals the risk: Cold War realism naturalizes strategic competition embargo as natural law derivable from anarchy, when historical evidence (post-Cold War US-China technology competition despite ongoing strategic tension, negotiated détente, SALT frameworks) shows embargo is policy choice contingent on institutional will, not immutable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embargo_effectiveness_measurement,
    'Did the embargo actually retard Soviet technological development, or did internal systemic inefficiency account for most of the lag?',
    'Counterfactual analysis: compare Soviet achievement trajectories in embargoed sectors vs non-embargoed sectors; analysis of smuggling and black-market acquisition rates; technical analysis of production substitution vs genuine innovation gaps',
    'If embargo highly effective: constraint is genuine extraction with strong suppression mechanism. If low effectiveness: constraint is largely performative (piton classification gains empirical support). If effectiveness declining over time: supports scaffold sunset narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embargo_effectiveness_measurement, empirical, 'Whether embargo achieved stated technical denial goals').

omega_variable(
    satellite_state_benefit_extraction_balance,
    'Do Eastern Bloc satellite states experience net benefit or net extraction from participation in Soviet-led embargo countermeasures?',
    'Economic analysis of trade redirections: measurable gains from Soviet market access vs costs of embargo spillover; comparison of satellite living standards with non-aligned embargoed economies; analysis of military-industrial value capture',
    'If net extraction: satellite states should show stronger snare/tangled-rope classification and lower theater. If net benefit: should show rope classification, supporting tangled-rope parent constraint. If asymmetric (some states benefiting, others extracting): decompose into separate family constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(satellite_state_benefit_extraction_balance, empirical, 'Whether satellite states benefit from embargo regime participation').

omega_variable(
    alternative_coordination_sufficiency,
    'Could technology transfer restrictions achieve their stated security goals through transparent market mechanisms (tariffs, licensing) rather than embargo enforcement?',
    'Policy analysis of market-based alternatives; historical comparison with non-adversarial technology transfer controls; simulation of licensing vs embargo regime effectiveness',
    'If yes: current regime represents unnecessary extraction overlay on basic coordination problem. If no: suppression and enforcement are genuinely necessary, supporting tangled-rope classification. If partially: reveals how much of extraction is administrative overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, conceptual, 'Whether embargo is necessary or represents administrative overhead').

omega_variable(
    smuggling_black_market_scale,
    'What percentage of critical strategic technology actually reached Soviet acquisition via illegal channels, and how did this trend evolve?',
    'Declassified intelligence agency reports; technical reverse-engineering analysis of Soviet systems; trade statistics for third-party intermediaries; border customs data',
    'High and increasing smuggling rate: supports piton classification (degraded enforcement) and suggests low effective chi despite formal regime structure. Low smuggling: supports snare classification (effective suppression of access). Enables mapping of theater_ratio trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smuggling_black_market_scale, empirical, 'Scale and trajectory of technology transfer via smuggling').

omega_variable(
    western_internal_coherence,
    'How coherent was the Western coalition? Did all major allies enforce embargo consistently, or was enforcement selective?',
    'Historical analysis of embargo violations by NATO allies; Japanese and European technology transfer to Soviet bloc; investigation of Allied disagreements on strategic items lists; analysis of CoCom enforcement patterns',
    'If highly coherent: rope classification of Western beneficiaries is valid. If fragmented: Western beneficiaries experience significant constraint (tangled_rope), beneficiary group becomes more differentiated. Supports omega on whether embargo was genuine Western coordination or cover for partial defection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(western_internal_coherence, empirical, 'Coherence of Western embargo coalition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soviet_embargo_regime, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sovi_tr_t0, soviet_embargo_regime, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sovi_tr_t15, soviet_embargo_regime, theater_ratio, 15, 0.5).
narrative_ontology:measurement(sovi_tr_t30, soviet_embargo_regime, theater_ratio, 30, 0.58).
narrative_ontology:measurement(sovi_tr_t44, soviet_embargo_regime, theater_ratio, 44, 0.65).

% Extraction over time
narrative_ontology:measurement(sovi_be_t0, soviet_embargo_regime, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sovi_be_t15, soviet_embargo_regime, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(sovi_be_t30, soviet_embargo_regime, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(sovi_be_t44, soviet_embargo_regime, base_extractiveness, 44, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soviet_embargo_regime, global_infrastructure).
narrative_ontology:boltzmann_floor_override(soviet_embargo_regime, 0.25).
narrative_ontology:affects_constraint(soviet_embargo_regime, cocom_export_control_enforcement).
narrative_ontology:affects_constraint(soviet_embargo_regime, soviet_autarky_mobilization).
narrative_ontology:affects_constraint(soviet_embargo_regime, cold_war_security_dilemma).

% DUAL FORMULATION NOTE:
% Soviet embargo regime decomposes into three structurally distinct constraints: (1) CoCom enforcement apparatus (ε≈0.35, Rope/Piton family) — technology transfer restrictions coordinating Western alliance with degrading theater over time; (2) Soviet autarky mobilization (ε≈0.45, Tangled Rope) — state response coordinating domestic innovation while extracting from citizens; (3) broader Cold War security dilemma (ε≈0.70, Snare) — strategic competition driving mutual containment. The embargo regime is the surface intersection of these three. See family constraint stories for technical decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(soviet_embargo_regime, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
