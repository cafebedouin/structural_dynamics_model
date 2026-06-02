% ============================================================================
% CONSTRAINT STORY: cold_war_geopolitical_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cold_war_geopolitical_constraint, []).

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
 *   constraint_id: cold_war_geopolitical_constraint
 *   human_readable: Cold War Geopolitical Constraint: Superpower Bipolarity and Nuclear Stalemate
 *   domain: geopolitical/military/ideological
 *
 * SUMMARY:
 *   The Cold War geopolitical constraint represents a 46-year structural
 *   arrangement (1945-1991) between the Soviet Union and United States,
 *   characterized by ideological opposition, military competition, nuclear
 *   deterrence, and proxy warfare in the developing world. The constraint
 *   exhibits diagnostic complexity: from the perspective of superpower
 *   leadership, it functions as a coordination mechanism solving postwar
 *   order questions and alliance management. From the perspective of proxy
 *   war populations and nuclear-threatened civilians, it is pure extraction
 *   with no exit option. From the perspective of military-industrial
 *   establishments, it is entirely enabling. The theater_ratio increases over
 *   time as the functional coordination problems (preventing superpower
 *   direct war, managing sphere-of-influence stability) become obscured
 *   behind performative ideological rhetoric. By the 1970s-80s, both
 *   superpowers had internalized pragmatic coexistence logic (détente, arms
 *   control negotiations, cultural exchange) while maintaining public Cold
 *   War theater for domestic regime legitimacy. The constraint's dissolution
 *   in 1989-1991 reveals that the structural 'inevitability' attributed to it
 *   was partial — the realist mountain view naturalizes a contingent
 *   institutional arrangement. The measurable increase in theater_ratio from
 *   0.48 to 0.68 over the interval reflects growing gap between proclaimed
 *   ideological warfare and actual pragmatic superpower cooperation.
 *
 * KEY AGENTS:
 *   - Soviet Leadership: Powerful/mobile but constrained institutional actor — benefits from Cold War as regime legitimacy mechanism and military-industrial complex demand; constrained by unsustainable economic burden of arms race
 *   - American Leadership: Powerful/mobile but constrained institutional actor — benefits from Cold War as justification for global military presence and security apparatus expansion; constrained by fiscal strain and imperial overstretch
 *   - Military-Industrial Complex (both superpowers): Institutional/arbitrage beneficiary — extracts maximum value from sustained weapons competition and defense spending
 *   - NATO/Warsaw Pact Allied Leadership: Institutional/arbitrage beneficiary — captures security guarantees and patron-client leverage; constrained by superpower dominance of decision-making
 *   - Proxy War Populations (Korea, Vietnam, Afghanistan, etc.): Powerless/trapped victim — bears full cost of superpower competition through military occupation, resource extraction, civilian casualties
 *   - Civilian Populations Under Nuclear Threat: Powerless/trapped victim — subjected to existential threat with zero exit option; suppressed through psychological terror and resource diversion
 *   - Non-Aligned Movement: Organized/constrained actor — experiences mixed coordination benefits (diplomatic leverage, independence space) and extraction threats (coercion, intervention risk)
 *   - Developing Nations: Powerful but constrained — experiences mixed coordination (development aid, trade access) and extraction (political subordination, resource drain, proxy war costs)
 *   - Peace and Disarmament Movements: Organized/constrained actor — sees constraint as temporary with sunset pathway through institutional reform and arms control
 *   - Analytical Observer: Potential false naturalization of contingent arrangement as immutable geopolitical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cold_war_geopolitical_constraint, 0.58).
domain_priors:suppression_score(cold_war_geopolitical_constraint, 0.72).
domain_priors:theater_ratio(cold_war_geopolitical_constraint, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cold_war_geopolitical_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(cold_war_geopolitical_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cold_war_geopolitical_constraint, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cold_war_geopolitical_constraint, tangled_rope).
narrative_ontology:human_readable(cold_war_geopolitical_constraint, "Cold War Geopolitical Constraint: Superpower Bipolarity and Nuclear Stalemate").
narrative_ontology:topic_domain(cold_war_geopolitical_constraint, "geopolitical/military/ideological").

domain_priors:requires_active_enforcement(cold_war_geopolitical_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cold_war_geopolitical_constraint, soviet_leadership).
narrative_ontology:constraint_beneficiary(cold_war_geopolitical_constraint, american_leadership).
narrative_ontology:constraint_beneficiary(cold_war_geopolitical_constraint, military_industrial_complex).
narrative_ontology:constraint_beneficiary(cold_war_geopolitical_constraint, state_security_apparatus).
narrative_ontology:constraint_victim(cold_war_geopolitical_constraint, proxy_war_populations).
narrative_ontology:constraint_victim(cold_war_geopolitical_constraint, developing_nations).
narrative_ontology:constraint_victim(cold_war_geopolitical_constraint, civilian_populations_under_nuclear_threat).
narrative_ontology:constraint_victim(cold_war_geopolitical_constraint, global_economic_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROXY WAR POPULATIONS (SNARE) — Nations and peoples caught in proxy conflicts (Korea, Vietnam, Afghanistan, multiple African and Latin American states) face maximum extraction with zero exit options. These populations bear the full cost of superpower competition — military occupation, resource extraction, political coercion, and civilian casualties — while having no agency in the constraint structure. The bipolarity forces alignment: neutrality is punished, defection is militarily crushed. This is pure extraction masked as ideological choice.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS UNDER NUCLEAR THREAT (SNARE) — All citizens of nuclear-armed states and allied nations live under existential threat with no exit option and no consent. The threat is structurally maintained through deterrence doctrine and weapons accumulation. Extraction occurs through psychological terror (duck-and-cover drills, civil defense theater) and resource diversion from welfare to military spending. The threat itself is the mechanism of suppression — fear binds the population to state authority and ideological conformity.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-ALIGNED MOVEMENT (TANGLED ROPE) — Nations attempting genuine neutrality (Yugoslavia, India, Egypt at various periods) experience the constraint as mixed coordination and extraction. The non-aligned framework provides real coordination benefits: diplomatic leverage, economic negotiation space, ideological independence. But these benefits are constantly threatened by superpower pressure (sanctions, coups, military intervention). Exit costs are severe — defection risks invasion or economic collapse. The constraint provides some agency (the coordination function) while extracting through coercion (constant threat of retaliation for deviation).
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: NATO/WARSAW PACT LEADERSHIP (ROPE) — Allied institutional actors see the constraint primarily as a coordination mechanism for military alliance and ideological bloc formation. The bipolarity solves a collective action problem: it provides security guarantees, clear alliance structure, and shared enemy definition. These leaders experience the constraint as enabling rather than extractive. Their arbitrage option is substantial — they can leverage their position as key allies to extract concessions from the superpower patron. The extraction runs toward the periphery, not toward allied leadership.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: MILITARY-INDUSTRIAL COMPLEX (ROPE) — Defense contractors, military establishments, and security apparatus in both superpowers experience the Cold War as a coordination mechanism that solves their principal problem: sustained demand for weapons, military research, and defense infrastructure. The constraint is entirely enabling for this actor. The bipolarity justifies continuous weapons development, military spending, and technological competition. These institutions have maximum arbitrage — they can leverage threat perception to extract resources from the state. The constraint benefits this actor completely.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: IDEOLOGICAL STATE APPARATUS (PITON) — The theatrical framing of the Cold War as a clash of civilizations (capitalism vs communism, freedom vs totalitarianism, Western values vs Soviet imperialism) is substantially performative. By the 1970s-80s, both superpowers had internalized pragmatic realpolitik and détente logic, yet the ideological theater persisted to justify defense spending and state authority. The constraint maintains itself through narrative performance (propaganda, education systems, media framing) rather than through genuine ideological commitment. Theater_ratio is high because the functional reality (cartel-like superpower cooperation to prevent third-party nuclear proliferation and maintain sphere-of-influence stability) is hidden behind ideological performance.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DEVELOPING NATIONS (TANGLED ROPE) — Nations seeking economic development and political autonomy experience the constraint as both enabling and extractive. The bipolarity creates opportunities for leverage: playing superpowers against each other, securing development loans and military aid, extracting concessions by threatening defection to the other bloc. But these opportunities are constrained by real military and economic asymmetries. Development aid comes with political strings (Cold War alignment), resource extraction flows toward superpowers, and military intervention punishes deviation. The constraint provides genuine coordination benefits (access to trade, capital, technology) alongside substantial extraction (political subordination, resource drain, proxy war costs).
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: SUPERPOWER LEADERSHIP - SOVIET UNION (TANGLED ROPE) — The Soviet leadership experiences the constraint as both enabling and extractive. The Cold War provides narrative justification for party authority, military buildup, and suppression of internal dissent ('capitalist encirclement' justifies authoritarianism). But the constraint also extracts from the USSR: sustained military competition drains resources needed for economic development and consumer welfare, military-first ideology strangles innovation in civilian sectors, and the arms race creates fiscal unsustainability. The constraint has genuine coordination function (alliance maintenance, sphere-of-influence stability) alongside severe extraction (resource drain, systemic economic distortion). Exit is theoretically mobile but practically constrained by the regime's dependence on the Cold War enemy image for internal legitimacy.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: SUPERPOWER LEADERSHIP - UNITED STATES (TANGLED ROPE) — American leadership experiences the constraint as mixed coordination and extraction. Cold War bipolarity solves the postwar problem: how to maintain U.S. economic and military dominance while justifying permanent military-industrial mobilization. The constraint provides real coordination benefits: alliance formation, sphere-of-influence stability, justification for global military presence. But it also extracts from American society: resource diversion from domestic welfare to defense, imperial overstretch that distorts fiscal balance, militarization of foreign policy that produces costly interventions (Vietnam). Exit is theoretically mobile (the U.S. could choose deescalation) but constrained by domestic constituencies (military-industrial complex, anticommunist ideological coalition) dependent on Cold War continuation.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 10: PEACE AND DISARMAMENT MOVEMENTS (SCAFFOLD) — Peace activists, disarmament advocates, and anti-nuclear movements see the Cold War as a temporary constraint with a built-in sunset. This perspective recognizes the constraint as having genuine coordination problems (prevention of accidental nuclear war, management of proxy competition) but views them as solvable through institutional reform (arms control treaties, treaties, hotlines, verification mechanisms). The movements see arms control treaties (SALT I/II, ABM Treaty, NTP, Comprehensive Test Ban) as creating pathways to deescalation. Theater is moderate because the actual function (preventing superpower war and managing proxy conflicts) is partially visible. This perspective's sunset logic treats Cold War as a transient political arrangement rather than a permanent geopolitical law.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 11: ANALYTICAL OBSERVER - SYSTEMIC INEVITABILITY VIEW (MOUNTAIN) — From a civilizational/realist perspective, the Cold War bipolarity appears as an immutable consequence of the anarchic international system. Two nuclear superpowers with incompatible ideologies and competing spheres of influence necessarily generate confrontation, arms racing, and proxy warfare. This is seen as natural law in geopolitics — inevitable given the structural conditions. However, the mountain classification is diagnostically suspect. The constraint's high theater_ratio (0.68), extractiveness (0.58), and suppression (0.72) reveal that significant portions are contingent institutional arrangements (ideological theater, domestic political incentives, military-industrial lobbying) rather than structural inevitabilities. The 'realist' framing naturalizes what historical analysis reveals to be avoidable political choices.
constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cold_war_geopolitical_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cold_war_geopolitical_constraint, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cold_war_geopolitical_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cold_war_geopolitical_constraint, TR),
    TR >= 0.70.

:- end_tests(cold_war_geopolitical_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Cold War extracts substantially from proxy war populations and nuclear-threatened civilians through physical violence, political coercion, and resource diversion. But the extraction is not maximal (0.66+) because significant portions involve genuine coordination functions (alliance maintenance, sphere-of-influence stability, prevention of superpower direct war). The increase from 0.42 to 0.64 over the interval reflects increasing extraction as weapons accumulation accelerates and proxy wars expand (Vietnam peak in 1965-1973). Suppression (0.72): High. The constraint is maintained through multiple suppression mechanisms: military threat (nuclear deterrence, conventional arms), political coercion (alliance discipline, threat of intervention), ideological closure (both superpowers control information and education systems), and psychological terror (civil defense theater, threat of nuclear annihilation). Suppression is not total (0.80+) because significant portions of populations in both superpowers retain agency in limited domains. Theater_ratio (0.68): High. The functional core of the Cold War — preventing superpower direct war and managing global order — requires relatively modest institutional overhead. But the constraint is wrapped in elaborate ideological performance: anticommunism and anti-imperialism propaganda, education systems teaching existential threat, media framing, diplomatic theater. By 1975-1985, both superpowers had internalized pragmatic détente logic (arms control, cultural exchange, trade) while maintaining public Cold War rhetoric for domestic constituencies. The theater increases over time as actual coordination problems become stable and theater becomes the primary maintenance mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. Superpower leadership sees Tangled Rope (mixed coordination and extraction, with significant beneficiary positioning). Military-industrial complex sees Rope (pure coordination, entirely enabling). Allied leadership sees Rope (pure coordination, security guarantees). Proxy war populations see Snare (pure extraction, zero exit). Peace movements see Scaffold (temporary problem with sunset pathway). Analytical realists see Mountain (immutable structural necessity). The gap between superpower beneficiary view (Rope/Tangled Rope) and proxy war population view (Snare) is maximal: the same constraint appears enabling to one and purely extractive to the other. This perspectival gap is the diagnostic signature of the constraint's hybrid nature. The piton perspective (degraded theater) reveals that by the 1980s, the constraint was maintained primarily through institutional inertia and elite interest rather than genuine ideological commitment or functional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure is asymmetric across different agent classes. Superpower leadership (institutional/arbitrage) derives d ≈ 0.20-0.30 — they are net beneficiaries experiencing the constraint as enabling, with options to arbitrage their position. Military-industrial complexes derive d ≈ 0.10 — maximum beneficiary status, pure enabling actor. Allied institutional leadership derives d ≈ 0.25-0.35 — beneficiaries with arbitrage options, though constrained by superpower dominance. Proxy war populations derive d ≈ 0.95 — trapped victims bearing maximum extraction cost with zero exit. Nuclear-threatened civilians derive d ≈ 0.90 — highly victimized by threat imposition with constrained exit. Developing nations derive d ≈ 0.65-0.75 — mixed positioning: benefits from aid/trade access but victimized by political subordination and proxy war participation. Non-aligned movement derives d ≈ 0.55 — mixed victim-beneficiary status with constrained but meaningful exit options. Peace movements derive d ≈ 0.70 — victimized by threat imposition and resource diversion but possessing some organizational agency.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY WITH FALSE SUMMIT: The analytical observer's mountain perspective appears initially justified by realist theory — bipolarity seems like an immutable consequence of anarchic international structure with two nuclear superpowers and incompatible ideologies. However, structural data reveals this as a false summit (Goodhart signal). The theater_ratio (0.68) is too high for a natural law — genuine mountains have theater ratios near 0.0-0.1 because they function by necessity, not by performance. The contingency of Cold War termination (the constraint dissolved rapidly in 1989-1991 when superpower leaders chose deescalation) proves that the 'structural inevitability' was illusory. The realist naturalization of bipolarity obscures the political agency and choice involved in Cold War maintenance. The mandatrophy resolution: the constraint is NOT mountain but Tangled Rope from superpower leadership perspective (mixed coordination and extraction, with genuine beneficiary positioning that made continuation rational), misclassified as mountain by realist theory that confuses structural incentives with structural inevitability. Multiple dimensions are at play simultaneously: genuine coordination problems (preventing superpower direct war), genuine extraction mechanisms (military-industrial complex rent-seeking, proxy war resource drain), genuine suppression (nuclear threat, alliance discipline), and genuine theater (ideological performance decoupling from pragmatic behavior). The analytical observer must recognize that the 'inevitability' of Cold War is itself a narrativization used by beneficiary elites to justify extraction — a false natural law that obscures political choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superpower_cartel_or_genuine_conflict,
    'Is the Cold War a genuine ideological/military conflict or a superpower cartel managing global order to prevent third-party disruption?',
    'Analysis of superpower cooperation patterns: nuclear nonproliferation treaty jointly enforced, sphere-of-influence respect (Soviet tolerance of U.S. interventions in Latin America, U.S. tolerance of Soviet interventions in Eastern Europe), proxy war restraint (neither superpower directly engaged the other militarily despite multiple opportunities), arms control negotiation despite public hostility. Compare rhetoric (ideological warfare) to behavior (pragmatic cooperation on order maintenance).',
    'If genuine conflict: snare classification from proxy war populations is correct (they bear full cost of superpower adversarialism). If cartel: snare classification is even more severe (extraction occurs despite fake ideological justification that presumes real competition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(superpower_cartel_or_genuine_conflict, empirical, 'Whether Cold War is genuine ideological conflict or superpower cartel').

omega_variable(
    ideological_commitment_vs_regime_legitimacy,
    'Do superpower leaders genuinely believe their ideological positions or use ideology primarily for domestic regime legitimacy?',
    'Private correspondence analysis, declassified memoirs, internal policy documents revealing actual beliefs vs public rhetoric. Comparison of ideological purity in internal communications vs external propaganda. Analysis of policy choices that contradict stated ideology (U.S. support for authoritarian allies despite freedom rhetoric; Soviet pragmatism on market mechanisms in economic reform attempts).',
    'If genuine belief: ideology is a binding mechanism justifying extraction to true believers. If regime tool: ideology is pure theater (theater_ratio approaches 1.0), and the constraint is maintained through institutional inertia and elite interest rather than ideological commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ideological_commitment_vs_regime_legitimacy, empirical, 'Whether Cold War ideology reflects genuine belief or regime legitimacy tool').

omega_variable(
    proxy_war_necessity_in_constraint_structure,
    'Are proxy wars a necessary structural feature of the Cold War constraint or contingent choices by superpower leadership?',
    'Counterfactual analysis: examination of conflicts where superpowers refrained from intervention despite opportunity (Austria 1955, Yugoslavia breakup scenarios, some African conflicts). Comparison of crisis management protocols (hotlines, crisis communication, restraint agreements) showing that direct conflict was actively prevented while proxy conflicts were tolerated. Analysis of whether proxy wars served genuine deterrence function or primarily served military-industrial complex demand.',
    'If necessary: proxy war populations experience structural snare (extraction is inherent to the constraint). If contingent: proxy wars represent policy choice, and snare classification shifts from structural to created — higher moral responsibility for extraction lies with decision-makers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_necessity_in_constraint_structure, empirical, 'Whether proxy wars are structurally necessary or contingent policy choices').

omega_variable(
    nuclear_deterrence_stability_or_theater,
    'Does nuclear deterrence actually prevent superpower war (stabilizing function) or is deterrence doctrine primarily theatrical maintenance of superpower legitimacy?',
    'Examination of near-miss incidents (Cuban Missile Crisis, Able Archer 83, Korean Air Flight 007) showing either that deterrence logics actually prevented escalation or that accidents/miscommunication nearly triggered war despite deterrence. Analysis of whether military doctrine genuinely reduces war probability or primarily justifies weapon accumulation.',
    'If deterrence stabilizes: nuclear threat suppresses superpower direct conflict, justifying some extraction costs as insurance. If theatrical: nuclear doctrine is performance maintaining elite power and military spending, and suppression is psychological rather than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_deterrence_stability_or_theater, empirical, 'Whether nuclear deterrence provides actual stability or is theater').

omega_variable(
    exit_cost_trajectory_for_superpowers,
    'At what point does Cold War continuation become more expensive than exit for superpower elites?',
    'Comparative analysis of superpower resource diversion, economic growth distortions, technological innovation tradeoffs across Cold War period. Measurement of moment when Soviet economic system becomes unsustainable due to military overcommitment, when U.S. fiscal constraints bind due to defense spending, when both superpowers perceive arms control as economically advantageous.',
    'If exit costs become negative: constraint transitions from snare/tangled_rope to scaffold (temporary arrangement with sunset). This explains the actual outcome: 1989-1991 dissolution of Soviet system and Cold War rapid deescalation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_trajectory_for_superpowers, empirical, 'Economic breakdown point for Cold War maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cold_war_geopolitical_constraint, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coldwar_tr_t0, cold_war_geopolitical_constraint, theater_ratio, 0, 0.48).
narrative_ontology:measurement(coldwar_tr_t20, cold_war_geopolitical_constraint, theater_ratio, 20, 0.62).
narrative_ontology:measurement(coldwar_tr_t45, cold_war_geopolitical_constraint, theater_ratio, 45, 0.68).

% Extraction over time
narrative_ontology:measurement(coldwar_be_t0, cold_war_geopolitical_constraint, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(coldwar_be_t20, cold_war_geopolitical_constraint, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(coldwar_be_t45, cold_war_geopolitical_constraint, base_extractiveness, 45, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cold_war_geopolitical_constraint, enforcement_mechanism).
narrative_ontology:affects_constraint(cold_war_geopolitical_constraint, nuclear_deterrence_doctrine).
narrative_ontology:affects_constraint(cold_war_geopolitical_constraint, proxy_war_dynamics).
narrative_ontology:affects_constraint(cold_war_geopolitical_constraint, ideological_bloc_formation).
narrative_ontology:affects_constraint(cold_war_geopolitical_constraint, military_industrial_complex_expansion).

% DUAL FORMULATION NOTE:
% The Cold War decomposes into structurally distinct constraints: (1) nuclear deterrence as an enforcement mechanism preventing superpower direct war; (2) proxy war dynamics as extraction mechanism targeting developing nations; (3) ideological bloc coordination as alliance formation; (4) military-industrial complex expansion as rent-seeking mechanism. Each has distinct ε values. The overarching Cold War constraint story integrates these components but should be cross-referenced with the downstream stories for detailed analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cold_war_geopolitical_constraint, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
