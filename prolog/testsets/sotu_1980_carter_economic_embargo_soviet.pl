% ============================================================================
% CONSTRAINT STORY: sotu_1980_carter_economic_embargo_soviet
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1980_carter_economic_embargo_soviet, []).

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
 *   constraint_id: sotu_1980_carter_economic_embargo_soviet
 *   human_readable: Carter's 1980 Economic Embargo and Trade Restrictions on the Soviet Union
 *   domain: geopolitical/trade/sanctions
 *
 * SUMMARY:
 *   The Carter administration's economic embargo on the Soviet Union
 *   following the December 1979 invasion of Afghanistan creates a structural
 *   constraint that simultaneously functions as pure coordination (from the
 *   beneficiary's perspective), asymmetric extraction (from the target's
 *   perspective), and a tangled mix of both (from the perspective of those
 *   bearing domestic costs). The embargo comprises fishing permit
 *   revocations, technology equipment cutoffs, agricultural export bans, and
 *   Olympic participation restrictions. Its structural role is to create
 *   material consequences for military aggression outside military
 *   confrontation — a 'price mechanism' that coordinates Western response
 *   while extracting costs from the Soviet economy and American exporters.
 *   The constraint exhibits high suppression (total embargo with few
 *   alternatives) and moderate theater (the embargo is substantive policy,
 *   not purely performative, though some signaling function is performative
 *   relative to actual deterrent effect). The extractiveness value reflects
 *   that the embargo imposes real material costs while retaining some
 *   coordination rationale.
 *
 * KEY AGENTS:
 *   - Soviet Union: Primary target/victim (powerless/trapped) — faces comprehensive economic isolation with no exit except capitulation on Afghanistan; bears maximum extraction costs
 *   - U.S. Geopolitical Leadership: Primary beneficiary (institutional/arbitrage) — captures coalition coordination benefits, signaling resolve, and non-military deterrence mechanism; controls the constraint
 *   - U.S. Agricultural Exporters: Secondary victim (moderate/constrained) — lose Soviet market; face career/business disruption; also benefit from signaled deterrence and coalition messaging (tangled rope)
 *   - Western Allied Nations: Secondary beneficiary and victim (organized/constrained) — benefit from unified coalition messaging; bear cost of joining embargo and reduced trade options; face pressure to choose sides
 *   - Non-Aligned Nations: Tertiary actors (organized/constrained) — navigate pressure to support embargo while maintaining neutrality; benefit from trade arbitrage opportunities; bear geopolitical polarization costs
 *   - International Trade Regime (GATT): Institutional observer (institutional/arbitrage) — rules theoretically govern trade; embargo suspends rules; regime persists without reform (piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating embargo as inevitable law of geopolitics rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1980_carter_economic_embargo_soviet, 0.58).
domain_priors:suppression_score(sotu_1980_carter_economic_embargo_soviet, 0.72).
domain_priors:theater_ratio(sotu_1980_carter_economic_embargo_soviet, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1980_carter_economic_embargo_soviet, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1980_carter_economic_embargo_soviet, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1980_carter_economic_embargo_soviet, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1980_carter_economic_embargo_soviet, tangled_rope).
narrative_ontology:human_readable(sotu_1980_carter_economic_embargo_soviet, "Carter's 1980 Economic Embargo and Trade Restrictions on the Soviet Union").
narrative_ontology:topic_domain(sotu_1980_carter_economic_embargo_soviet, "geopolitical/trade/sanctions").

domain_priors:requires_active_enforcement(sotu_1980_carter_economic_embargo_soviet).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1980_carter_economic_embargo_soviet, u_s_geopolitical_leverage).
narrative_ontology:constraint_beneficiary(sotu_1980_carter_economic_embargo_soviet, western_allied_coalition).
narrative_ontology:constraint_victim(sotu_1980_carter_economic_embargo_soviet, soviet_economic_access).
narrative_ontology:constraint_victim(sotu_1980_carter_economic_embargo_soviet, u_s_agricultural_exporters).
narrative_ontology:constraint_victim(sotu_1980_carter_economic_embargo_soviet, u_s_fishing_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET ECONOMIC ACCESS (SNARE) — No exit from the embargo without conceding on Afghanistan. Technology cutoffs, agricultural bans, and fishing permit revocation are comprehensive barriers with no alternative supply routes. The constraint extracts maximum cost with zero coordination benefit for the target. Suppression is total: exit requires capitulation on core security claims.
constraint_indexing:constraint_classification(sotu_1980_carter_economic_embargo_soviet, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: U.S. AGRICULTURAL AND FISHING EXPORTERS (TANGLED ROPE) — Bear direct extraction through lost markets and revoked permits. Simultaneously benefit from coalition coordination messaging and (indirectly) from geopolitical risk reduction if the embargo deters Soviet expansion. High costs; genuine but non-obvious coordination benefit. Exit is constrained by government policy, not impossible — some exporters shift to other markets, but at significant loss.
constraint_indexing:constraint_classification(sotu_1980_carter_economic_embargo_soviet, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. GEOPOLITICAL LEVERAGE AND ALLIED COALITION (ROPE) — Pure coordination mechanism. The embargo creates concrete material consequences that unify Western alliance response, signal resolve to allies, and coordinate military-free pressure on Soviet decision-making. No extraction experienced by this agent; the constraint creates the goods they want (coalition cohesion, non-military deterrence). Exit is arbitrary (can abandon embargo any time) because this agent controls the constraint.
constraint_indexing:constraint_classification(sotu_1980_carter_economic_embargo_soviet, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-ALIGNED AND ALLIED NATIONS (TANGLED ROPE) — Allied nations experience enforcement pressure to join the embargo (coordinating allied response, genuine coordination benefit). Non-aligned nations face implicit pressure to choose sides while officially maintaining neutrality (extraction). Both benefit from signaling coalition strength; both bear costs of reduced trade options and geopolitical polarization. Exit is constrained by alliance relationships and economic retaliation risk.
constraint_indexing:constraint_classification(sotu_1980_carter_economic_embargo_soviet, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL TRADE REGIME (PITON) — GATT/most-favored-nation rules ostensibly govern trade relationships. The embargo suspends these rules entirely, yet the trade regime persists without fundamental reform. The embargo is treated as an exception ('national security') rather than evidence that the regime cannot enforce its own principles. The institutional machinery of trade law becomes performative — functioning to ratify decisions made outside its framework rather than to constrain them. High theater (the regime appears to govern but doesn't) with degraded functional capacity.
constraint_indexing:constraint_classification(sotu_1980_carter_economic_embargo_soviet, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, embargo represents an immutable property of state relations: when military aggression occurs, economic coercion is the only non-kinetic response available to nation-states. The constraint appears as a natural law of geopolitics — inevitable, unchangeable, inherent to the system. However, this naturalizes what is actually a contingent institutional choice: the West *could* have negotiated, engaged, or accepted the invasion without economic response. The mountain classification masks agency and normative choice as structural inevitability.
constraint_indexing:constraint_classification(sotu_1980_carter_economic_embargo_soviet, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1980_carter_economic_embargo_soviet_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1980_carter_economic_embargo_soviet, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1980_carter_economic_embargo_soviet, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1980_carter_economic_embargo_soviet, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1980_carter_economic_embargo_soviet, TR),
    TR >= 0.70.

:- end_tests(sotu_1980_carter_economic_embargo_soviet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting that the embargo imposes real material costs on the Soviet economy and U.S. exporters while serving a genuine coordination function (unifying Western response). The value increased from 0.42 to 0.58 over the interval as third-party circumvention became apparent and domestic American costs accumulated. If the embargo were purely extractive (no coordination benefit), ε would approach 0.70+; if purely coordinative, ε would be ≤0.05. The intermediate value reflects the hybrid: coordination is real (allied coalition messaging, military-free deterrence), but so is extraction (Soviet access severely constrained, exporters bear costs). Suppression (0.72): High. The embargo is comprehensive — technology cutoffs leave no alternative suppliers for advanced equipment; agricultural bans target primary export; fishing permit revocation is total. Soviet alternatives are limited by existing isolation; U.S. exporters have few substitute markets for products designed for Soviet sale. The suppression is maintained by government enforcement, not by market scarcity, but the barrier is substantial. Theater ratio (0.38): Moderate. The embargo is substantive policy with real material effects, not pure performance. However, approximately 38% of its force is theater: signaling resolve, demonstrating unity, showing cost without military escalation. The true deterrent effect on Soviet decision-making is uncertain; the signaling function is certain. As substitution channels develop and third-party trade circumvents the embargo, theater_ratio would rise (more signaling relative to actual constraint).
 *
 * PERSPECTIVAL GAP:
 *   The embargo demonstrates maximal perspectival divergence. The U.S. geopolitical leadership sees pure coordination (Rope) — they are solving the legitimate problem of unified Western response to invasion without military escalation. The Soviet Union sees pure extraction (Snare) — it is isolated with no alternative and no coordination benefit. U.S. exporters see tangled rope (mixed coordination and extraction) — they lose markets but benefit from deterrence signaling. Allied nations see tangled rope (mixed enforcement pressure and coordination benefit). The international trade regime sees piton (its rules are suspended yet persist as theater). The civilizational analytical observer risks seeing mountain (embargo as inevitable law of geopolitics) — but the structural data reveals this as false summit: the embargo is a constructed choice by identifiable beneficiaries (U.S. leadership, Western coalition) that serves their interests, not an immutable property of state relations. The perspectival gap is diagnostic: it reveals the constraint's structure more clearly than any single perspective could.
 *
 * DIRECTIONALITY LOGIC:
 *   Soviet Union (powerless/trapped/national): d = 0.98. Derives from victim status (full target of embargo) + trapped exit (no way to rejoin global trade without conceding on Afghanistan) + national scope (embargo is explicitly targeted at this nation). Applies the sigmoid f(d) to produce high experienced extractiveness χ. The Snare classification follows from high d, high f(d), and high suppression. U.S. Geopolitical Leadership (institutional/arbitrage/global): d = 0.02. Derives from beneficiary status (embargo is their tool) + arbitrage exit (can abandon embargo any time) + global scope (operates at civilizational level). Produces negative effective extraction — this agent experiences the constraint as pure benefit. Rope classification follows. U.S. Agricultural Exporters (moderate/constrained/national): d = 0.72. Derives from victim status (lose markets) + constrained exit (cannot abandon U.S. policy; could shift to other markets at cost) + national scope. High experienced extraction but with some coordination benefit (deterrence signal). Tangled Rope classification. Non-aligned nations (organized/constrained/global): d = 0.58. Derive from mixed beneficiary/victim status (can profit through circumvention, bear political pressure) + constrained exit (cannot openly defy U.S. pressure without cost; cannot fully comply without abandoning trade opportunities) + global scope. Tangled Rope classification reflects the mixed position.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: The embargo resolves the mandatrophy by showing that 'which type is this?' is the wrong question. The constraint IS all six types simultaneously, from different structural positions. The mandatrophy is resolved by recognizing that classification is position-relative and that the perspectival divergence itself is the analytical output. The U.S. beneficiary experiences Rope (pure coordination). The Soviet victim experiences Snare (pure extraction). The domestic exporters experience Tangled Rope (mixed costs and benefits). The allied nations experience Tangled Rope (enforcement pressure with coordination rationale). The trade regime experiences Piton (rules become theater). The civilizational observer risks Mountain (natural law) — but false summit detection reveals this as naturalization of contingent choice. No single type is 'the truth'; the presheaf over the observation site IS the answer. The mandatrophy is resolved by accepting that different observers measure different things and that reconciling their measurements reveals the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embargo_effectiveness_ambiguity,
    'Does the embargo actually deter Soviet military behavior or merely signal Western resolve while allowing the invasion to proceed unchanged?',
    'Counterfactual analysis of Soviet decision-making; correlation between embargo severity and Soviet policy reversals; historical comparison with other sanctions regimes and their strategic outcomes',
    'If deterrent: embargo is primarily coordinating tool (rope from beneficiary view validated). If performative: embargo functions as extraction mechanism (snare from Soviet view, piton from trade regime view confirmed). Misclassification leads to strategic failure in subsequent sanctions design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embargo_effectiveness_ambiguity, empirical, 'Whether embargo achieves deterrence or functions as performative signaling').

omega_variable(
    domestic_coalition_stability,
    'Does the embargo maintain domestic U.S. political coalition behind geopolitical containment or does cost to exporters fracture support?',
    'Congressional voting records and debate on embargo extension; tracking of export lobby pressure and trade association positions; electoral analysis of swing states dependent on agricultural/fishing exports',
    'If coalition stable: embargo is sustainable constraint (tangled rope for exporters is bearable). If coalition fractures: exporters shift to snare perspective and embargo becomes unsustainable (requires pure coercion to enforce domestically). Signals whether tangled rope can remain stable over time or degrades to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_coalition_stability, empirical, 'Whether domestic coalition sustains embargo or fractures under exporter pressure').

omega_variable(
    third_party_circumvention_capacity,
    'Can non-aligned and neutral nations effectively circumvent the embargo by trading with Soviet Union as intermediaries, nullifying extraction?',
    'Trade flow analysis through transshipment nodes (Europe, Middle East); correlation between embargo severity and third-party trade volumes with USSR; tracking of re-export pricing and supply chain substitution',
    'If circumvention effective: embargo is more performative than extractive (theater rises, effective extraction falls). Snare perspective becomes unstable — Soviet access constrained but not fully severed. Theater_ratio rises and classification may shift toward piton or rope depending on whether third-party trade is coordinated evasion or natural market response.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(third_party_circumvention_capacity, empirical, 'Whether third-party trade circumvents embargo effectiveness').

omega_variable(
    allied_coherence_and_compliance,
    'Do allied nations actually comply with the embargo or do they negotiate exceptions and backchannels that undermine coalition messaging?',
    'Comparison of stated embargo scope with actual trade flows; investigation of bilateral exceptions (Japan, Western Europe); tracking of technology transfer circumvention',
    'If compliance strong: rope perspective (pure coordination) is validated. If compliance weak: tangled rope perspective (extraction with hidden coordination) is confirmed. Weak compliance shifts classification toward piton (performative theater masking divergent interests). Affects whether embargo is sustainable multistate constraint or fragile coalition artifact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allied_coherence_and_compliance, empirical, 'Whether allied nations genuinely comply with embargo or negotiate exceptions').

omega_variable(
    natural_law_vs_constructed_choice,
    'Is the embargo an inevitable response to military aggression (natural law) or a contingent geopolitical choice that could have taken alternative forms (constructed constraint)?',
    'Historical analysis of alternative responses available (negotiations, engagement, acceptance, military response); comparative study of other invasion scenarios and non-embargo responses; philosophical interrogation of whether ''economic pressure is inevitable'' versus ''this specific embargo was chosen''',
    'If natural law: mountain classification is justified. If constructed: false summit signature should fire, reclassifying as tangled rope with beneficiaries (U.S. leverage, allied coalition). This determines whether the constraint is a law of geopolitics or a policy choice that serves identifiable interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_choice, conceptual, 'Whether embargo is natural law response or contingent policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1980_carter_economic_embargo_soviet, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_embargo_tr_t0, sotu_1980_carter_economic_embargo_soviet, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sotu_embargo_tr_t2, sotu_1980_carter_economic_embargo_soviet, theater_ratio, 2, 0.34).
narrative_ontology:measurement(sotu_embargo_tr_t5, sotu_1980_carter_economic_embargo_soviet, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(sotu_embargo_be_t0, sotu_1980_carter_economic_embargo_soviet, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sotu_embargo_be_t2, sotu_1980_carter_economic_embargo_soviet, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(sotu_embargo_be_t5, sotu_1980_carter_economic_embargo_soviet, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1980_carter_economic_embargo_soviet, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1980_carter_economic_embargo_soviet, strategic_grain_embargo_agricultural_sector).
narrative_ontology:affects_constraint(sotu_1980_carter_economic_embargo_soviet, soviet_technology_isolation_defense_industrial).
narrative_ontology:affects_constraint(sotu_1980_carter_economic_embargo_soviet, olympic_boycott_soft_power).
narrative_ontology:affects_constraint(sotu_1980_carter_economic_embargo_soviet, cold_war_alliance_cohesion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1980_carter_economic_embargo_soviet, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
