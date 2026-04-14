% ============================================================================
% CONSTRAINT STORY: sotu_1956_eisenhower_collective_security_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1956_eisenhower_collective_security_system, []).

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
 *   constraint_id: sotu_1956_eisenhower_collective_security_system
 *   human_readable: NATO Collective Security Alliance System (1956 Eisenhower Framework)
 *   domain: military/geopolitical/institutional
 *
 * SUMMARY:
 *   The NATO Collective Security System established through the 1949 NATO
 *   Treaty and formalized under Eisenhower's 1956 Strategic Policy represents
 *   a fundamental restructuring of Western defense posture. Rather than
 *   unilateral U.S. military expansion, the alliance pools defensive
 *   capability across member states through integrated command structures,
 *   burden-sharing treaties, and collective deterrence doctrine. From the
 *   U.S. perspective, this distributes defense costs and creates
 *   institutional mechanisms for containing Soviet expansion. From European
 *   perspectives, it provides a security umbrella against Soviet aggression
 *   and enables economic reconstruction without unilateral rearmament.
 *   However, the alliance simultaneously concentrates deterrence authority in
 *   U.S. hands (nuclear monopoly through 1954), imposes defense spending
 *   obligations on member populations, constrains policy autonomy of
 *   peripheral members (Greece, Turkey, Portugal), and maintains elaborate
 *   institutional apparatus whose performative content exceeds functional
 *   necessity. The constraint exhibits all six classification types from
 *   different structural positions, making it a diagnostic exemplar for
 *   geopolitical tangled ropes.
 *
 * KEY AGENTS:
 *   - Taxpayer populations (mostly West European): Primary victims (powerless/trapped) — bear defense spending burden, conscription costs, with no exit capacity or negotiating power
 *   - Peripheral NATO members (Greece, Turkey, Portugal): Secondary victims (moderate/constrained) — gain security guarantees but face sovereignty constraints, basing obligations, and disproportionate cost-sharing
 *   - U.S. security establishment (DoD, Joint Chiefs, NATO command): Primary beneficiary (institutional/arbitrage) — controls alliance architecture, operates at maximum institutional advantage, experiences constraint as pure coordination mechanism
 *   - Core European NATO members (West Germany, UK, France): Secondary beneficiary (institutional/arbitrage) — gain U.S. nuclear umbrella and voice in alliance, but constrained by geopolitical exposure and alliance obligations
 *   - Non-aligned nations and Soviet bloc: Organized opposition (organized/constrained) — see NATO as temporary bipolar structure with built-in sunset logic
 *   - NATO bureaucratic apparatus: Institutional actor (institutional/arbitrage) — maintains alliance coordination rituals; sees own function as substantially theatrical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1956_eisenhower_collective_security_system, 0.52).
domain_priors:suppression_score(sotu_1956_eisenhower_collective_security_system, 0.48).
domain_priors:theater_ratio(sotu_1956_eisenhower_collective_security_system, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1956_eisenhower_collective_security_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1956_eisenhower_collective_security_system, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1956_eisenhower_collective_security_system, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1956_eisenhower_collective_security_system, tangled_rope).
narrative_ontology:human_readable(sotu_1956_eisenhower_collective_security_system, "NATO Collective Security Alliance System (1956 Eisenhower Framework)").
narrative_ontology:topic_domain(sotu_1956_eisenhower_collective_security_system, "military/geopolitical/institutional").

domain_priors:requires_active_enforcement(sotu_1956_eisenhower_collective_security_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_collective_security_system, west_european_states).
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_collective_security_system, united_states_security_establishment).
narrative_ontology:constraint_beneficiary(sotu_1956_eisenhower_collective_security_system, alliance_core_institutional_apparatus).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_collective_security_system, taxpayer_populations).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_collective_security_system, peripheral_member_states).
narrative_ontology:constraint_victim(sotu_1956_eisenhower_collective_security_system, non_aligned_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED TAXPAYER (SNARE) — Trapped by national conscription and tax obligation. Bears defense spending costs with no exit or negotiation capacity. Experiences high suppression (legal conscription, tax collection) and asymmetric extraction (pays for alliance maintenance, receives only abstract deterrence benefit). Cannot exit or arbitrage.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PERIPHERAL MEMBER STATE (TANGLED ROPE) — Constrained by geopolitical exposure and NATO conditionality. Gains security umbrella and access to alliance institutions, but faces coordination demands (force contributions, basing rights) and asymmetric cost-sharing. Mixed extraction: some benefits (deterrence), some costs (sovereignty constraints). Exit is possible but expensive (geopolitical isolation).
constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: U.S. SECURITY ESTABLISHMENT (ROPE) — Institutional beneficiary with maximum arbitrage. Controls alliance architecture, NATO command structure, and military doctrine. Experiences the constraint as pure coordination: distributing defense burden across NATO members enables U.S. to project power globally while members bear proportional costs. Generates career advancement, budgetary justification, and institutional expansion. Operates at positive d (full beneficiary).
constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CORE EUROPEAN NATO MEMBER (ROPE) — Institutional beneficiary. Gains security guarantees, economic reconstruction support (Marshall Plan), and voice in alliance decisions. Exit costs are high (Soviet exposure) but arbitrage is real (can negotiate terms, can threaten withdrawal). Experiences the constraint as coordination with asymmetric benefit: gets U.S. nuclear umbrella while U.S. gets forward basing and containment logic.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: NON-ALIGNED & SOVIET OPPOSITION (SCAFFOLD) — Organized agents outside the alliance system see NATO as a temporary structure with built-in sunset logic. Soviet collapse is expected, bipolar deterrence will dissolve, and the alliance will either transform or fragment. Constrained by bipolarity but with genuine strategic alternatives. Experiences the constraint as temporary coordination framework with generational lifecycle.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NATO BUREAUCRATIC INSTITUTION (PITON) — The alliance apparatus itself is substantially theatrical by the 1950s-60s. Command structures exist but actual deterrence is credible only because of U.S. nuclear capability, not because of integrated NATO forces. The alliance maintains elaborate coordination rituals (military meetings, treaty reviews, defense ministerial conferences) whose performative value exceeds functional content. Theater persists through institutional inertia — NATO claims to solve coordination but actual deterrence is unilateral U.S. nuclear guarantee. Theater_ratio is high because the alliance's legitimacy depends on appearing multilateral while actual power is concentrated.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational perspective, NATO is a genuine hybrid: it coordinates legitimate collective defense (genuine coordination value) while simultaneously extracting compliance from members and concentrating deterrence authority in U.S./core institutions. The constraint both solves a coordination problem (distributed defense is cheaper than unilateral buildout) AND enables asymmetric extraction (peripheral states pay, core benefits; taxpayers pay, security establishment controls). Not a mountain (coordination function is real but not inevitable), not pure rope (suppression and asymmetric benefit are high), not pure snare (genuine coordination value exists). The analytical classification is tangled_rope because both mechanisms are structurally necessary and both perspectives are rational.
constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1956_eisenhower_collective_security_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1956_eisenhower_collective_security_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1956_eisenhower_collective_security_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1956_eisenhower_collective_security_system, TR),
    TR >= 0.70.

:- end_tests(sotu_1956_eisenhower_collective_security_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over the interval. Initial value (0.38) reflects 1954-1956 period when NATO was still establishing force structure and burden-sharing was still being negotiated. By 1960 (time_point=6), extractiveness rises to 0.54 as institutional arrangements solidify and burden-sharing asymmetries become structural. The extraction flow is not pure rent-seeking: genuine coordination benefits exist (distributed defense costs are lower than unilateral buildout). But the extraction is real: U.S. security establishment gains institutional power and budgetary justification; core European members gain security at peripheral members' cost; all benefit at taxpayers' expense. Suppression (0.48): Moderate. Taxpayer populations are legally obligated to fund defense spending and conscription is mandatory in European NATO members, but suppression is not total — domestic political opposition to NATO exists (particularly in peripheral members) and is not entirely suppressed. Treaty obligations constrain member states, but exit is theoretically possible at geopolitical cost. Theater ratio (0.58): High. NATO maintains elaborate multilateral institutional apparatus (integrated commands, treaty reviews, ministerial meetings) whose coordination function exceeds actual operational necessity. Deterrence is credible primarily because of U.S. nuclear capability, not because of integrated NATO conventional forces. The alliance's legitimacy depends on appearing multilateral while actual power concentration is unilateral. Theater rises slightly over interval as institutional apparatus expands.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim is the diagnostic feature of this constraint. The U.S. security establishment sees Rope — a pure coordination mechanism that solves the collective action problem of distributed defense. They genuinely experience the constraint as coordination because they designed it and benefit from its institutional structure. The taxpayer sees Snare — pure extraction with high suppression and no exit. They bear costs without negotiating power or meaningful benefit. The peripheral member sees Tangled Rope — both genuine coordination (security guarantee) and extraction (cost-sharing asymmetry, sovereignty constraints). The core European member sees Rope with high benefits and low extraction costs. The NATO institutional apparatus sees itself as Piton — a theatrical structure maintained by inertia because alternatives haven't fully replaced it. The analytical observer sees the full structure as Tangled Rope: genuine coordination function (solving collective defense problem) coexists with asymmetric extraction (core benefits more than periphery, beneficiaries benefit more than taxpayers). The gap reveals that the 'pure coordination' framing used by architects and beneficiaries naturalizes the distributional asymmetries that make the constraint extractive for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the alliance. Taxpayers have no beneficiary position and cannot exit — they bear costs with full directionality toward victimhood (d ≈ 0.92 for trapped agents). U.S. security establishment has maximum beneficiary position and full arbitrage capacity — they benefit from alliance maintenance while controlling institutional design (d ≈ 0.05 for institutional beneficiary). Core European NATO members have mixed position: partial beneficiaries (security guarantees), partial victims (defense spending, sovereignty constraints), constrained exit (geopolitical exposure) — they occupy the interior of the directionality spectrum (d ≈ 0.50). Peripheral members have higher directionality toward victimhood because their security gains are proportionally smaller and their sovereignty constraints are higher (d ≈ 0.68 for moderate constrained agents bearing disproportionate costs). The analytical observer's directionality is canonical for analytical power in institutional context with global scope (d ≈ 0.72).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by establishing that the coordination function and extraction mechanism are structurally coupled, not contradictory. NATO genuinely solves a collective action problem: unilateral European rearmament would trigger arms race dynamics and Soviet acceleration. Distributed defense through alliance mechanisms is more efficient than bilateral U.S.-European agreements. This is real coordination value. However, the alliance's design simultaneously enables asymmetric benefit distribution: U.S. gains institutional control, core European members gain security guarantees, peripheral members and taxpayers bear disproportionate costs. The solution to the collective action problem does not require this particular distribution. Alternative designs (proportional burden-sharing, genuinely integrated command, rotating leadership) would preserve coordination while reducing asymmetric extraction. The mandate resolves because both coordination and extraction are necessary to explain the constraint's actual structure and persistence. It is not 'NATO is coordination pretending to be extraction' or vice versa — it is 'NATO is coordination that enables and masks extraction.' The institutional design choices (U.S. control of nuclear doctrine, asymmetric burden-sharing, alliance obligation without proportional benefit guarantee) are not required by coordination logic; they are extraction mechanisms layered onto coordination structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_credibility_mechanism,
    'Is NATO deterrence credible because of integrated multilateral force structure, or solely because of U.S. nuclear guarantee?',
    'Historical counterfactual: would Soviet behavior have differed if U.S. nuclear commitment were uncertain while NATO conventional forces were identical? Analysis of NATO force deployment vs. U.S. strategic doctrine evolution; identification of decisions made by collective NATO authority vs. U.S. unilateral authority.',
    'If credibility derives from integrated structure: NATO coordination function is genuine and extraction is reduced. If credibility derives from U.S. nuclear monopoly: NATO''s coordination value is theater, and extraction is higher (members pay for institution whose actual function is U.S. deterrence). Classification implications: shifts from Tangled Rope toward Snare under high-extraction interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_credibility_mechanism, empirical, 'Whether NATO deterrence credibility derives from multilateral structure or U.S. nuclear capability').

omega_variable(
    cost_sharing_asymmetry_intent,
    'Is NATO''s cost-sharing asymmetry (U.S. bears disproportionate defense burden) intentional alliance design or emerging institutional drift?',
    'Eisenhower speeches and strategic doctrine (1954-1961); NATO treaty and burden-sharing formulas; U.S. defense spending as percentage of alliance total over time; evidence of U.S. pressure for increased European contributions vs. U.S. unwillingness to reduce its own commitment.',
    'If intentional design: U.S. accepts cost asymmetry as price of European integration and containment strategy. Tangled Rope with high coordination value. If drift/capture: U.S. security establishment extracts from alliance while appearing to service collective defense. Shifts toward Snare interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_sharing_asymmetry_intent, empirical, 'Whether NATO cost-sharing asymmetry is intentional design or institutional drift').

omega_variable(
    suppression_mechanism_structural,
    'Is NATO suppression (constraining member exit) structural (geopolitical isolation cost) or enforced (treaty obligation + U.S. coercion)?',
    'Counterfactual analysis: if treaty obligations were eliminated but geopolitical context unchanged, would members exit? Analysis of withdrawal attempts (France 1966) and U.S. response; comparison of formal treaty text vs. actual enforcement mechanisms.',
    'If structural (geopolitical): suppression is high but legitimate coordination cost. If enforced (coercive): suppression represents extractive mechanism. Implications for classification: affects whether suppression metric reflects genuine coordination cost or asymmetric coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural, empirical, 'Whether NATO member suppression is structural or enforced').

omega_variable(
    peripheral_member_benefit_distribution,
    'Do peripheral NATO members (Greece, Turkey, Portugal) receive security benefits proportional to their contributions, or does the alliance extract disproportionately from periphery?',
    'Cost-benefit analysis: comparison of defense spending by peripheral vs. core members; analysis of NATO security guarantees'' credibility for peripheral members vs. core; historical cases of alliance support for threatened periphery (Hungary 1956, Czechoslovakia 1968 — both gaps).',
    'If benefits proportional: all members are intermediate Tangled Rope with varying parameters. If disproportionate extraction: periphery sees Snare, core sees Rope. Classification implications: affects whether to model as single constraint or decompose into separate stories per member category.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peripheral_member_benefit_distribution, empirical, 'Distribution of NATO security benefits between core and peripheral members').

omega_variable(
    institutional_theater_ratio_trend,
    'Is the NATO institutional apparatus''s theater_ratio (performative vs. functional activity) increasing or stable over the Eisenhower period (1954-1961)?',
    'Content analysis of NATO official communications; comparison of planned military exercises vs. executed exercises; tracking of NATO command meetings vs. NATO-directed operational decisions; measurement of press coverage of NATO coordination vs. actual military coordination events.',
    'If increasing: NATO is drifting toward Piton classification (performative institution maintained by inertia). If stable: theater_ratio is constant feature of alliance, not degradation signal. Affects trajectory analysis and lifecycle assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_theater_ratio_trend, empirical, 'Trend in NATO institutional theater_ratio during Eisenhower period').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1956_eisenhower_collective_security_system, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nato_cs_tr_t0, sotu_1956_eisenhower_collective_security_system, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nato_cs_tr_t3, sotu_1956_eisenhower_collective_security_system, theater_ratio, 3, 0.54).
narrative_ontology:measurement(nato_cs_tr_t6, sotu_1956_eisenhower_collective_security_system, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(nato_cs_be_t0, sotu_1956_eisenhower_collective_security_system, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nato_cs_be_t3, sotu_1956_eisenhower_collective_security_system, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(nato_cs_be_t6, sotu_1956_eisenhower_collective_security_system, base_extractiveness, 6, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1956_eisenhower_collective_security_system, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_collective_security_system, soviet_expansion_deterrence).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_collective_security_system, european_rearmament_arms_race).
narrative_ontology:affects_constraint(sotu_1956_eisenhower_collective_security_system, u_s_military_industrial_expansion).

% DUAL FORMULATION NOTE:
% The NATO collective security system coordinates upstream constraints (European rearmament, Soviet deterrence) while itself becoming subject to institutional drift that generates downstream effects (NATO institutional expansion, burden-sharing asymmetries, peripheral state subordination). Decomposition would separate coordination function (genuine collective defense solution) from extraction mechanism (institutional design choices that enable asymmetric benefit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1956_eisenhower_collective_security_system, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
