% ============================================================================
% CONSTRAINT STORY: sotu_1951_truman_korean_war_collective_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1951_truman_korean_war_collective_defense, []).

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
 *   constraint_id: sotu_1951_truman_korean_war_collective_defense
 *   human_readable: UN Collective Defense Against Soviet Proxy Aggression (Korea 1950-1951)
 *   domain: military/geopolitics
 *
 * SUMMARY:
 *   The Korean War represents a critical moment in Cold War institutional
 *   design: the first operational test of collective security through UN
 *   machinery against Soviet proxy aggression. President Truman's 1951 SOTU
 *   framing positions US military intervention as a structural necessity—if
 *   Korea falls unchallenged, Soviet expansionism faces no institutional cost
 *   and can proceed to absorb remaining free nations. The constraint operates
 *   on two levels: (1) the coordination level—establishing that Soviet proxy
 *   aggression triggers coordinated institutional response deters future
 *   Soviet moves; (2) the extraction level—achieving this coordination
 *   requires extracting massive resources from American society (military
 *   personnel, defense budget, technological capacity) and imposing
 *   catastrophic costs on Korean civilians and soldiers. The constraint is
 *   genuinely hybrid: it solves a real collective security problem
 *   (coordinating Western response to Soviet expansion) while simultaneously
 *   extracting wealth and imposing mortality on actors who bear the costs.
 *   The theater ratio reflects that the Cold War narrative surrounding the
 *   intervention contains significant performative content—the framing of
 *   Soviet intentions as part of a coordinated global expansionist strategy
 *   involves interpretation and threat inflation alongside genuine strategic
 *   concern.
 *
 * KEY AGENTS:
 *   - United States Government: Primary beneficiary (institutional/arbitrage) — gains operational control of UN collective security machinery and establishes deterrence credibility
 *   - American Military Personnel: Primary victim (powerless/trapped) — bears mortality, injury, and war trauma costs with no exit option
 *   - American Public and Defense Industry: Secondary actor (organized/constrained) — organized through taxation and procurement; benefits from defense spending but also bears opportunity cost of military expenditure
 *   - South Korea: Primary beneficiary but also victim (powerful/mobile) — survives as a nation through US intervention but experiences warfare devastation and becomes dependent on US military presence
 *   - Free World Alliance: Secondary beneficiary (powerful/arbitrage) — benefits from deterrence of Soviet expansion and establishment of institutional response mechanism
 *   - Korean Civilians: Primary victim (powerless/trapped) — experience warfare, displacement, bombardment, and death without agency in the constraint structure
 *   - Soviet Union: Tertiary actor (institutional/arbitrage) — proxy through North Korea; the constraint attempts to establish cost for this actor's expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1951_truman_korean_war_collective_defense, 0.58).
domain_priors:suppression_score(sotu_1951_truman_korean_war_collective_defense, 0.72).
domain_priors:theater_ratio(sotu_1951_truman_korean_war_collective_defense, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1951_truman_korean_war_collective_defense, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1951_truman_korean_war_collective_defense, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1951_truman_korean_war_collective_defense, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1951_truman_korean_war_collective_defense, tangled_rope).
narrative_ontology:human_readable(sotu_1951_truman_korean_war_collective_defense, "UN Collective Defense Against Soviet Proxy Aggression (Korea 1950-1951)").
narrative_ontology:topic_domain(sotu_1951_truman_korean_war_collective_defense, "military/geopolitics").

domain_priors:requires_active_enforcement(sotu_1951_truman_korean_war_collective_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1951_truman_korean_war_collective_defense, south_korea).
narrative_ontology:constraint_beneficiary(sotu_1951_truman_korean_war_collective_defense, free_world_alliance).
narrative_ontology:constraint_beneficiary(sotu_1951_truman_korean_war_collective_defense, us_institutional_credibility).
narrative_ontology:constraint_victim(sotu_1951_truman_korean_war_collective_defense, american_military_personnel).
narrative_ontology:constraint_victim(sotu_1951_truman_korean_war_collective_defense, american_defense_budget).
narrative_ontology:constraint_victim(sotu_1951_truman_korean_war_collective_defense, allied_military_personnel).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMERICAN MILITARY PERSONNEL (SNARE) — Conscripted or volunteered into a commitment that, once made, admits no exit. The soldier bears the full mortality and injury cost of the constraint with no meaningful choice to withdraw. Trapped by military law, chain of command, and patriotic obligation. Maximum experienced extraction — the soldier's body is the mechanism through which institutional commitments are enforced.
constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AMERICAN PUBLIC AND DEFENSE INDUSTRY (TANGLED ROPE) — Organized through taxation and procurement but also benefits from defense spending, employment, technological spinoffs, and deterrence credibility. Genuine coordination function (collective security against Soviet expansion) coexists with asymmetric extraction (reallocation of resources away from domestic social spending). Exit cost is high (perceived loss of security and international credibility) but not total—alternative defense postures are conceivable.
constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US GOVERNMENT AND UN FRAMEWORK (ROPE) — Benefits from operational control of the constraint through UN Security Council authorization and military command structure. Experiences the constraint as pure coordination: establishing institutional machinery for responding to aggression creates the deterrence credibility that prevents future Soviet moves. The constraint enables rather than extracts from this agent.
constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOUTH KOREA (TANGLED ROPE) — Primary beneficiary of the collective defense mechanism—without US intervention, the nation ceases to exist as a political entity. But the benefit comes at cost: economic devastation from warfare, dependence on US military protection and economic aid, limited sovereign decision-making during the war, and long-term military garrison. Exit options exist (armistice, eventual negotiation) but at the cost of potentially abandoning the nation to absorption. Mixed coordination and extraction.
constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: FREE WORLD ALLIANCE (ROPE) — NATO and non-aligned free nations benefit from establishing that Soviet expansion triggers coordinated institutional response. The collective defense mechanism creates deterrence structure against Soviet territorial ambitions elsewhere. Perceived as pure coordination for this agent—the constraint solves the free world's coordination problem against Soviet aggression.
constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: KOREAN CIVILIANS (SNARE) — Bear the maximum cost of the constraint with zero agency. Experiencing military invasion, warfare, bombardment, displacement, and death. The institutional commitment to collective defense operates through their suffering without consultation or exit option. The constraint is realized through civilian casualty and displacement.
constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears as a natural and inevitable law of geopolitics: expansionist powers must be met with coordinated resistance or they will continue expanding. The iron logic of balance-of-power theory makes Soviet proxy aggression triggering collective defense response appear as immutable geopolitical law. However, structural data suggests this is a false summit—the 'inevitability' naturalizes what are actually political choices about how to respond to aggression.
constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1951_truman_korean_war_collective_defense_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1951_truman_korean_war_collective_defense, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1951_truman_korean_war_collective_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1951_truman_korean_war_collective_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. At constraint inception (T=0), extractiveness is moderate (0.35) because the collective security mechanism is novel and the military commitment is initially perceived as temporary and manageable. As the war extends beyond initial expectations (T=6-12), extractiveness rises sharply (0.58-0.62) as the resource and personnel commitment becomes evidently large and sustained. The rising trajectory reflects that the constraint's true extraction cost only becomes apparent over time as the coordination mechanism requires escalating sacrifice. Suppression (0.72): High. Multiple mechanisms suppress alternatives: (1) Institutional—UN Security Council authorization frames intervention as collective obligation, not choice; (2) Ideological—Cold War framing of Soviet expansion as existential threat leaves no space for non-military responses; (3) Military—once committed, exit from military engagement is politically costly and strategically dangerous; (4) Psychological—patriotic obligation and peer pressure suppress questioning of the commitment. Theater ratio (0.48): Moderate and rising. The Cold War narrative contains genuine strategic assessment (Soviet expansion is real) but also significant interpretation and threat inflation (degree of Soviet coordination, inevitability of the threat, necessity of military response). The theater increases over time (0.32 → 0.55) as the constraint settles into rhetorical justification and bureaucratic maintenance rather than acute response to immediate threat.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The American soldier sees pure extraction (Snare)—no coordination benefit accrues to the soldier, only mortality risk. The American public sees mixed coordination and extraction (Tangled Rope)—the defense spending and employment benefits are real, but opportunity costs are substantial. The US government sees pure coordination (Rope)—the mechanism solves the government's strategic problem of establishing credible deterrence. South Korea sees mixed benefit and cost (Tangled Rope)—survival as a nation is genuine benefit, but dependence on US military presence and warfare devastation are extraction. The Free World alliance sees pure coordination (Rope)—collective defense mechanism benefits all members through deterrence. Korean civilians see pure extraction (Snare)—no benefit accrues to the civilian population, only warfare casualties and displacement. The analytical observer risks seeing immutable geopolitical law (Mountain)—balance-of-power theory makes collective defense against expansionism appear necessary. The perspectival gaps reveal that the constraint's classification depends entirely on the observer's structural position: beneficiary sees coordination; victim sees extraction; organized actors see mixed benefit; powerless actors see pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: beneficiary/victim status and exit options. The US government (beneficiary + arbitrage exit) derives d ≈ 0.15, producing low f(d) and negative χ—the constraint benefits this agent. American soldiers (victim + trapped exit) derive d ≈ 0.95, producing high f(d) ≈ 1.42 and high χ—the constraint extracts from this agent maximally. The American public (beneficiary/victim hybrid + constrained exit) derives d ≈ 0.55, producing moderate f(d) ≈ 0.75 and moderate χ—the constraint has mixed effects. South Korea (beneficiary + mobile exit) derives d ≈ 0.40, producing moderate f(d) ≈ 0.40 and moderate χ—the nation benefits on balance but at cost. Korean civilians (victim + trapped exit) derive d ≈ 0.98, producing high f(d) ≈ 1.40 and high χ—the constraint extracts maximally from civilians. The scope modifier σ(global) = 1.2 amplifies effective extractiveness across all perspectives, reflecting that global scope makes verification and escape more difficult. The beneficiary's arbitrage options (US government can exit the UN commitment through political choice, though at cost) keep effective extraction moderate despite high scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid: it solves a real coordination problem (establishing that Soviet proxy aggression triggers institutional response) while simultaneously extracting resources and imposing casualties. The resolution requires clarifying which question is being answered: (1) DOES collective defense against expansionism achieve coordination? YES—the mechanism deters Soviet moves elsewhere and establishes institutional precedent for responding to aggression. (2) DOES the military response in Korea maximize efficiency in deterrence? UNKNOWN—alternative response modalities might achieve equivalent deterrence at lower cost (omega variables address this). (3) DOES the constraint's structure align costs and benefits? NO—massive costs fall on powerless agents (soldiers, civilians) while benefits accrue to institutional actors (governments, alliance structures). The mandatrophy is NOT resolved by claiming the constraint is 'really' one type (Rope or Snare). It is resolved by accepting that it is genuinely Tangled Rope: the coordination function is real and necessary, the extraction is also real and substantial, and the perspectival gap between beneficiary and victim is not a measurement error but a structural feature of how the constraint allocates costs and benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soviet_proxy_versus_direct_intent,
    'Did Soviet leadership intend the Korean invasion as the first step of a coordinated expansionist campaign, or was it a regional opportunistic action by Kim Il-sung with Soviet acquiescence?',
    'Historical analysis of Soviet diplomatic cables, Kremlin meeting records (to the extent available), Soviet strategic doctrine statements; assessment of whether Korea invasion fits pattern of coordinated Soviet strategy or represents tacit permission for regional actor initiative',
    'If coordinated Soviet expansionism: the mountain perspective (inevitable collective security response) is justified—Soviet power genuinely threatens global order and requires institutional response. If regional opportunism: the constraint is more contingent—collective defense choice reflects Western threat assessment rather than objective geopolitical law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_proxy_versus_direct_intent, empirical, 'Whether Korea invasion was coordinated Soviet strategy or regional opportunism').

omega_variable(
    alternative_response_sufficiency,
    'Would containment through economic isolation, diplomatic pressure, and defensive garrison (without offensive military response) have achieved equivalent deterrence against Soviet expansion?',
    'Counterfactual analysis using historical precedent of successful containment elsewhere (Berlin airlift, later Cuban missile crisis); assessment of whether Soviet decision-making was responsive to military costs or to diplomatic signals; comparison of deterrence credibility produced by military response versus other response modalities',
    'If alternative responses sufficient: the constraint''s extractiveness can be reclassified as unnecessary—the military response is extraction masquerading as necessary coordination. If military response uniquely necessary: the constraint''s tangled rope classification is confirmed—genuine coordination function justifies the extraction cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_response_sufficiency, conceptual, 'Whether non-military containment would achieve equivalent deterrence').

omega_variable(
    american_capacity_for_limited_commitment,
    'Could the United States have sustained effective collective defense against Soviet proxy expansion with lower extraction from American personnel and budget—through technology-intensive deterrence, allied burden-sharing, or graduated response rather than full-scale military commitment?',
    'Analysis of Korean War military expenditure as percentage of national budget; comparison with later Cold War deterrence mechanisms (nuclear strategy, NATO burden-sharing); assessment of whether Soviet perception of American commitment required the specific sacrifice profile that emerged or whether equivalent deterrence was achievable at lower cost',
    'If lower-cost deterrence possible: the extractiveness value is inflated—the constraint reflects institutional choice toward high-cost response rather than necessity. If high sacrifice required: the extractiveness correctly reflects the structural cost of maintaining credible institutional response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(american_capacity_for_limited_commitment, empirical, 'Whether lower-cost deterrence could achieve equivalent effect').

omega_variable(
    false_summit_institutional_inevitability,
    'Is the collective defense mechanism a natural law of geopolitics (mountain), or is it a political choice by the US and allies to institutionalize a particular response to Soviet expansion?',
    'Historical analysis of alternative institutional choices made at critical junctures (1950 UN Security Council vote, 1951 MacArthur controversy); demonstration that different political choices would have produced different institutional structures; identification of beneficiaries of the ''natural law'' framing',
    'If natural law: the constraint is genuinely immutable and objectively necessary. If political choice: the constraint is contingent on institutional design and could be redesigned; the ''naturalness'' framing serves the interests of actors who benefit from the current institutional structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_institutional_inevitability, conceptual, 'Whether collective defense is geopolitical law or political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1951_truman_korean_war_collective_defense, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1951_truman_korean_war_collective_defense, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sotu_tr_t6, sotu_1951_truman_korean_war_collective_defense, theater_ratio, 6, 0.48).
narrative_ontology:measurement(sotu_tr_t12, sotu_1951_truman_korean_war_collective_defense, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1951_truman_korean_war_collective_defense, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t6, sotu_1951_truman_korean_war_collective_defense, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(sotu_be_t12, sotu_1951_truman_korean_war_collective_defense, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1951_truman_korean_war_collective_defense, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1951_truman_korean_war_collective_defense, nato_alliance_credibility).
narrative_ontology:affects_constraint(sotu_1951_truman_korean_war_collective_defense, cold_war_deterrence_doctrine).
narrative_ontology:affects_constraint(sotu_1951_truman_korean_war_collective_defense, united_nations_authority_in_security).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1951_truman_korean_war_collective_defense, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
