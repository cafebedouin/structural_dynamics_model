% ============================================================================
% CONSTRAINT STORY: north_korea_songun_mandatrophy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_north_korea_songun_mandatrophy, []).

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
 *   constraint_id: north_korea_songun_mandatrophy
 *   human_readable: North Korean Songun (Military-First) Policy
 *   domain: political/economic
 *
 * SUMMARY:
 *   The Songun (Military-First) policy is North Korea's state doctrine
 *   mandating absolute prioritization of the Korean People's Army in all
 *   resource allocation, governance decisions, and social organization.
 *   Formalized ideologically by Kim Jong-il in 1994 (though military primacy
 *   dated from state founding in 1948), Songun has functioned as a control
 *   mechanism that concentrates extraction power in the military-security
 *   complex while maintaining a coordinative facade of national defense
 *   necessity. The constraint exhibits the signature features of a Snare:
 *   high base extractiveness (0.78), severe suppression (0.88 — absolute
 *   state monopoly on all economic activity, borders sealed, information
 *   controlled), and significant theater (0.64 — ideological justification
 *   through military parades, revolutionary history narrative, threat
 *   inflation). The theater ratio approaching piton threshold (0.64 near
 *   0.70) indicates that Songun has increasingly become performative
 *   justification for extraction rather than response to genuine military
 *   threat; actual military threat levels declined post-Cold War while
 *   extraction intensified. The mandatrophy is resolved by recognizing that
 *   all perspectives coexist legitimately: the civilian population genuinely
 *   experiences a Snare; the military officer corps genuinely experiences a
 *   Rope; the security apparatus genuinely experiences a beneficial
 *   coordination mechanism; the international sanctions regime creates a
 *   Tangled Rope dynamic; and observers risk naturalizing this as an
 *   immutable mountain when it is a contingent policy choice formalized only
 *   in 1994. The constraint's severity derives from total state monopoly (no
 *   exit options for civilians), asymmetric information control (suppression
 *   enforced by secret police), and decoupling of stated threat level from
 *   actual military requirements.
 *
 * KEY AGENTS:
 *   - North Korean Civilian Population: Primary victim (powerless/trapped) — bears extraction via food rationing, industrial collapse, infrastructure neglect; no exit option from closed state
 *   - Peasant Farming Collective: Primary victim (powerless/trapped) — grain requisitions for military take priority over civilian nutrition; state monopoly on agricultural output prevents alternative markets
 *   - Industrial Workforce: Secondary victim (moderate/constrained) — compulsory labor system; factories operate at minimal capacity due to energy/material diversion to military; limited internal mobility but no escape
 *   - Korean People's Army Officer Corps: Primary beneficiary (institutional/arbitrage) — guaranteed resource flows, prestige, institutional autonomy; access to special markets and foreign trade networks
 *   - State Security Apparatus: Secondary beneficiary (institutional/arbitrage) — Songun provides ideological justification for surveillance and control; access to special rations, black markets, offshore accounts
 *   - Party-State Ideological System: Institutional maintainer (institutional/arbitrage) — perpetuates Songun narrative through media, parades, education; theater ratio indicates degrading functional legitimacy
 *   - International Sanctions Regime: External powerful actor (powerful/mobile) — creates Tangled Rope by attempting coordination (export controls) while enabling regime concentration of power
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summit by naturalizing contingent policy as immutable law of Korean history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_korea_songun_mandatrophy, 0.78).
domain_priors:suppression_score(north_korea_songun_mandatrophy, 0.88).
domain_priors:theater_ratio(north_korea_songun_mandatrophy, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, extractiveness, 0.78).
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(north_korea_songun_mandatrophy, snare).
narrative_ontology:human_readable(north_korea_songun_mandatrophy, "North Korean Songun (Military-First) Policy").
narrative_ontology:topic_domain(north_korea_songun_mandatrophy, "political/economic").

domain_priors:requires_active_enforcement(north_korea_songun_mandatrophy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(north_korea_songun_mandatrophy, korean_people_army_officer_corps).
narrative_ontology:constraint_beneficiary(north_korea_songun_mandatrophy, state_security_apparatus).
narrative_ontology:constraint_victim(north_korea_songun_mandatrophy, civilian_population).
narrative_ontology:constraint_victim(north_korea_songun_mandatrophy, peasant_farming_collective).
narrative_ontology:constraint_victim(north_korea_songun_mandatrophy, industrial_workforce).
narrative_ontology:constraint_victim(north_korea_songun_mandatrophy, economic_growth_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped within the state apparatus with no exit option. Bears full cost of military-first resource allocation: chronic food insecurity, industrial collapse, infrastructure decay. State monopoly on all economic activity and borders prevents exit. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈1.10. Pure extraction with maximal coercion.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PEASANT FARMING COLLECTIVE (SNARE) — Trapped by state control of all agricultural output. Songun diverts fertilizer, fuel, and equipment to military production. Grain requisitions for military take priority over civilian nutrition. No alternative market, no exit. d≈0.97, f(d)≈1.42, σ=1.0 → χ≈1.11. Maximal extraction of productive capacity.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRIAL WORKFORCE (SNARE) — Constrained by compulsory labor system and ideological mobilization requirements. Factories operate at 20-30% capacity due to energy shortages and material diversion to military. Wages are nominal; work is mandatory. Limited exit (internal relocation) but no escape from system. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.90. High extraction with some structural mobility.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: KPA OFFICER CORPS (ROPE) — Primary beneficiary. Songun guarantees resource flows, prestige, and institutional autonomy. Officers experience the policy as coordinating state priorities, not as extraction. Arbitrage exits (senior officers access special markets, foreign trade networks for military goods). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.08. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE SECURITY APPARATUS (ROPE) — Secondary beneficiary. Songun provides ideological justification for surveillance, control, and repression. Security services access special food rations, markets, and resources in exchange for enforcing compliance. Arbitrage access to black market and offshore accounts. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PARTY-STATE IDEOLOGICAL SYSTEM (PITON) — Songun was formalized as state doctrine by Kim Jong-il (1994) but originates in military-centric governance since Korea's partition (1945). Theater ratio 0.64 reflects that Songun is now largely performative justification for military resource extraction, decoupled from actual security threats (threat level has declined since 1990s; extraction has intensified). Ideological framing persists through inertia despite declining functional legitimacy. Maintenance of the ritual (military parades, revolutionary history lectures, Songun media) sustains institutional control even as actual military capability has degraded. d≈0.12, f(d)≈-0.05, σ=1.0 → χ≈-0.03. Piton gate: theater_ratio (0.64) ≥ 0.70 boundary approached.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: INTERNATIONAL SANCTIONS REGIME (TANGLED ROPE) — Powerful external actor (UN, US, allies) attempting to constrain DPRK through coordination (export controls, financial restrictions) but simultaneously enabling extraction by concentrating regime power through isolation. Sanctions create asymmetric dependence on military for internal control. Mobile exit (sanctions can be lifted) but require regime behavioral change. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.56. Hybrid mechanism: coordination intent + extraction effect.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing Songun as an immutable feature of Korean historical inevitability or as a natural response to partition/threat. Temptation to frame military prioritization as inherent to survival in divided peninsula. However, structural data (ε=0.78, suppression=0.88, theater=0.64) contradicts mountain classification. The engine's false summit detector will flag this: Songun is a contingent institutional policy choice (formalized 1994), not a natural law. Theater ratio approaching piton threshold suggests intentional maintenance rather than natural emergence.
constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_korea_songun_mandatrophy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(north_korea_songun_mandatrophy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(north_korea_songun_mandatrophy, TR),
    TR >= 0.70.

:- end_tests(north_korea_songun_mandatrophy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. Songun mandates military priority in all resource allocation. Military budget estimates: 15-25% of state spending (vs global average 2-4%). More critically, military receives first access to food, energy, rare materials, foreign exchange, and skilled labor. The trajectory shows acceleration: 1945 (0.35, post-liberation military consolidation) → 1970 (0.58, Cold War peak) → 1994 (0.72, Songun formalization) → 2010 (0.78, post-sanctions intensification). The acceleration post-1994 reflects that Songun became ideologically justified extraction decoupled from actual threat-response. The value 0.78 reflects sustained high extraction without corresponding security gains (NK military capability has degraded relative to SK; nuclear weapons are compensatory technology, not core Songun driver). Suppression (0.88): Severe. North Korea operates a total state monopoly on all economic activity (90% of economy is state-owned). Borders are sealed with capital punishment for attempted exit. All information is controlled; foreign media banned; internet access restricted to elite. Labor is mandatory; internal movement requires papers (songbun caste system); freedom of association is zero. This is among the highest suppression scores in the DR corpus, exceeded only by historical totalitarian states at peak coercion. Theater ratio (0.64): Moderate-high. Songun's justification narrative emphasizes external military threat (US imperialism, South Korean aggression), but actual threat levels have declined post-Cold War. The constraint persists through performative maintenance: military parades, revolutionary history education (Songun ideology lectures in schools), propaganda media. However, the gap between stated threat and actual military requirements suggests that approximately 64% of the ongoing Songun apparatus is ritualistic justification rather than functional response. The theater ratio approaching piton threshold (0.70) indicates that if Songun continues unchanged for another decade without genuine external threat materialization, it will cross into pure inertial maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence across the DR classification system. The civilian population (powerless/trapped) sees a Snare — pure extraction with no coordination benefit, no exit option, maximal suppression. The beneficiary military sees a Rope — coordination mechanism that aligns state priorities with military institutional interests. The security apparatus sees a beneficial regime, perhaps even Scaffold if they perceive the sunset (regime succession could dismantle Songun). The international observer sees a Tangled Rope — coordination intent (sanctions attempting to constrain NK behavior) enabling extraction effect (sanctions concentrate regime power). The false natural law perspective naturalizes Songun as immutable response to geographic/historical necessity, but the theatrical character and post-1994 acceleration reveal this as a contingent institutional choice. The perspectival gap reflects the total inversion of beneficiary/victim relationships: those with power experience coordination; those without experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian population: Victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction because complete absence of exit options (sealed borders, state monopoly on employment, information control). Peasant collective: Victim + trapped → d≈0.97, f(d)≈1.42. Highest d because food requisition is non-negotiable and life-survival critical. Industrial workforce: Victim + constrained → d≈0.85, f(d)≈1.15. High d but not maximal because compulsory labor has limited internal mobility options and black markets partially mitigate official shortages. Military officer corps: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Arbitrage exits (access to special markets, foreign trade networks, dual economy of elites). International sanctions regime: Powerful + mobile → d≈0.52, f(d)≈0.68. Mobile exit (sanctions can be lifted if regime changes behavior) but powerful structural position (ability to impose costs). Party-state system: Institutional + arbitrage → d≈0.12, f(d)≈-0.05. Maintains regime benefits through ideological system; arbitrage access to state resources.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via perspectival multiplicity. The mandatrophy question — 'Is Songun coordination (Rope) or extraction (Snare)?' — is resolved by recognizing that it is BOTH simultaneously from different observational positions. For the beneficiary military, it is coordination (Rope) — alignment of state and institutional interests. For the trapped civilian, it is extraction (Snare) — pure coercive taking with no reciprocal benefit. For the security apparatus, it is a beneficial regime (Rope or early Scaffold if succession-aware). The misclassification risk was framing Songun as either purely extraction OR purely coordination at the state level. The framework shows that at the macro level (state interests), Songun appears as coordination; at the micro level (civilian experience), it is extraction. The theater ratio (0.64) escalates the mandatrophy: Songun began as functional military prioritization (1945-1970, responding to actual Cold War threat and war-devastated economy) but has become increasingly performative (1994-2010, as threat declined and extraction intensified despite degrading military capability). The policy persists through ritualistic maintenance — Songun lectures, military parades, revolutionary history education — that creates a gap between stated function and actual behavior. This gap is the signature of an institution approaching piton status (degraded function, maintained through theater). Songun is a Snare that has developed piton characteristics, not yet crossed the piton threshold (theater still below 0.70) but approaching it. Resolution: Classify as Snare (dominant type) with piton trending (theater rising, functional legitimacy declining). Successor regime decision point (post-Kim Jong Un) will determine whether Songun persists as entrenched Snare or transitions to Scaffold (temporary) then dissolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_threat_legitimacy,
    'What is the actual military threat level that would justify Songun''s resource extraction ratio?',
    'Comparative analysis: threat levels during Cold War vs post-1990 period; correlation between threat assessment and military spending; independent military capability assessment (not regime claims)',
    'If threat justifies current ratio: Snare classification downscales to Tangled Rope (extraction + legitimate coordination). If threat is significantly lower than claimed: Snare classification confirmed; extraction is decoupled from legitimate security need.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_threat_legitimacy, empirical, 'Whether actual threat levels justify claimed military extraction ratio').

omega_variable(
    regime_succession_stability,
    'Can Songun policy survive regime transition after Kim Jong Un''s death, or is it inherently tied to dynastic legitimacy?',
    'Analysis of policy statements from regime; comparison with succession crises in other military-first states; assessment of alternative legitimacy narratives available to successor regime',
    'If Songun is decoupled from dynasty: policy may persist under successor, entrenching constraint. If tied to Kim family legitimacy: successor may dismantle policy, resolving constraint. Classification would shift from persistent Snare to temporary Scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_succession_stability, conceptual, 'Whether Songun can survive regime succession without Kim family dynasty').

omega_variable(
    black_market_mitigation_efficacy,
    'To what extent do informal black markets and smuggling mitigate the severity of civilian extraction under Songun?',
    'Ethnographic study of informal markets; tracking of household income sources from defector accounts; estimation of goods flowing through black market vs official allocation; assessment of regime enforcement against informal trade',
    'If black markets significantly reduce extraction burden: d values for civilian populations decrease (exit becomes ''mobile'' rather than ''trapped''), classification could downshift to Tangled Rope. If enforcement is successful in suppressing markets: Snare classification confirmed; suppression scores increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_mitigation_efficacy, empirical, 'Whether informal markets provide effective mitigation to Songun extraction').

omega_variable(
    international_support_dependency,
    'Does Chinese military and economic support constitute a hidden beneficiary tier that structures Songun as a proxy for sino-korean asymmetry?',
    'Analysis of military aid flows; assessment of Chinese leverage over DPRK military decisions; correlation between Chinese strategic interests and Songun emphasis periods; examination of alternative alliance scenarios',
    'If China is hidden beneficiary: constraint decomposes into dual-structure (DPRK internal extraction + Sino-Korean asymmetry). Separate story needed for international dimension. If support is marginal: internal extraction logic dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_support_dependency, empirical, 'Whether Chinese support makes China a hidden beneficiary in Songun structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(north_korea_songun_mandatrophy, 1945, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(songun_tr_t1945, north_korea_songun_mandatrophy, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(songun_tr_t1970, north_korea_songun_mandatrophy, theater_ratio, 1970, 0.45).
narrative_ontology:measurement(songun_tr_t1994, north_korea_songun_mandatrophy, theater_ratio, 1994, 0.58).
narrative_ontology:measurement(songun_tr_t2010, north_korea_songun_mandatrophy, theater_ratio, 2010, 0.64).

% Extraction over time
narrative_ontology:measurement(songun_be_t1945, north_korea_songun_mandatrophy, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(songun_be_t1970, north_korea_songun_mandatrophy, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(songun_be_t1994, north_korea_songun_mandatrophy, base_extractiveness, 1994, 0.72).
narrative_ontology:measurement(songun_be_t2010, north_korea_songun_mandatrophy, base_extractiveness, 2010, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(north_korea_songun_mandatrophy, enforcement_mechanism).
narrative_ontology:affects_constraint(north_korea_songun_mandatrophy, korean_peninsula_partition_equilibrium).
narrative_ontology:affects_constraint(north_korea_songun_mandatrophy, sino_korean_asymmetric_dependency).
narrative_ontology:affects_constraint(north_korea_songun_mandatrophy, dprk_sanctions_extraction).
narrative_ontology:affects_constraint(north_korea_songun_mandatrophy, defector_escape_suppression).

% DUAL FORMULATION NOTE:
% Songun is distinct from but structurally dependent on Korea partition equilibrium (which provides threat narrative justification) and Chinese military support (which enables isolation strategy). Each upstream constraint provides legitimation for Songun; Songun in turn enables extraction mechanisms in downstream sanctions regime and defector suppression apparatus. These form a constraint cluster where Songun functions as a coordination hub for multiple extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(north_korea_songun_mandatrophy, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
