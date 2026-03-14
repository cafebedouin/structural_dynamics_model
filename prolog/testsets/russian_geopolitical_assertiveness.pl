% ============================================================================
% CONSTRAINT STORY: russian_geopolitical_assertiveness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russian_geopolitical_assertiveness, []).

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
 *   constraint_id: russian_geopolitical_assertiveness
 *   human_readable: Russian Geopolitical Assertiveness and Regional Constraint
 *   domain: geopolitical/international_relations
 *
 * SUMMARY:
 *   Russian geopolitical assertiveness represents a sustained extraction
 *   mechanism operating across multiple neighboring states and the broader
 *   international system. The constraint's structure involves coercive
 *   influence projection backed by military capability, energy leverage, and
 *   historical claims to regional primacy. The primary beneficiary (Russian
 *   state apparatus) extracts compliance, security buffers, and economic
 *   advantages from neighboring states while simultaneously constrained by
 *   NATO alliance strength, nuclear parity, and economic sanctions. The
 *   constraint operates through a combination of direct coercion (military
 *   deployments, territorial claims), economic leverage (energy dependency),
 *   and institutional lock-in (bloc-based diplomacy). The perspectival gap
 *   reveals how the same structural phenomenon appears as an immutable law of
 *   great power competition (analytical mountain), a temporary institutional
 *   artifact (piton), a coordination mechanism with asymmetric extraction
 *   (tangled rope), pure extraction for trapped neighbors (snare), a solvable
 *   problem through new institutions (scaffold), and rational state behavior
 *   for the beneficiary (rope). The extractiveness trajectory (0.42 → 0.68
 *   over 15 years) shows cumulative ratcheting of assertiveness through
 *   crisis cycles and failed diplomatic resolution. Theater ratio rise (0.38
 *   → 0.55) indicates increasing performative content — military exercises,
 *   rhetorical escalation, and diplomatic summits that maintain the
 *   constraint through signaling rather than through structural necessity.
 *   The constraint exhibits characteristics of all six DR types, making it a
 *   diagnostic exemplar for understanding how indexical classification
 *   captures perspectival divergence in high-stakes geopolitical domains.
 *
 * KEY AGENTS:
 *   - Russian State Apparatus: Primary beneficiary (institutional/arbitrage) — extracts geopolitical influence, security buffers, resource access; maintains enforcement capability through military and economic leverage
 *   - Neighboring Sovereign States: Primary victims (powerless/trapped) — bear extraction costs through constrained sovereignty, energy dependency, military threat; lack exit options due to geography and power asymmetry
 *   - NATO Alliance / Western States: Secondary organized actors (organized/constrained) — experience mixed coordination (deterrence framework) and extraction (bifurcated economy, military spending, bifurcated global order); constrained by alliance commitments and nuclear parity
 *   - European Energy Markets: Tertiary victim (institutional/constrained) — dependent on energy leverage; high switching costs; subject to disruption and price manipulation
 *   - International Order Reformers: Powerful state coalitions (powerful/mobile) — BRICS, multipolar frameworks, alternative institutions; see constraint as temporary feature of transition; have exit pathways through institutional reform
 *   - Cold War Legacy Institution: Institutional persistence mechanism (institutional/arbitrage) — NATO, nuclear deterrence, bloc diplomacy persist through inertia; maintain constraint through repetition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as structural inevitabilities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russian_geopolitical_assertiveness, 0.68).
domain_priors:suppression_score(russian_geopolitical_assertiveness, 0.72).
domain_priors:theater_ratio(russian_geopolitical_assertiveness, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russian_geopolitical_assertiveness, extractiveness, 0.68).
narrative_ontology:constraint_metric(russian_geopolitical_assertiveness, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(russian_geopolitical_assertiveness, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russian_geopolitical_assertiveness, snare).
narrative_ontology:human_readable(russian_geopolitical_assertiveness, "Russian Geopolitical Assertiveness and Regional Constraint").
narrative_ontology:topic_domain(russian_geopolitical_assertiveness, "geopolitical/international_relations").

domain_priors:requires_active_enforcement(russian_geopolitical_assertiveness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(russian_geopolitical_assertiveness, russian_state_apparatus).
narrative_ontology:constraint_beneficiary(russian_geopolitical_assertiveness, allied_regional_actors).
narrative_ontology:constraint_victim(russian_geopolitical_assertiveness, neighboring_sovereign_states).
narrative_ontology:constraint_victim(russian_geopolitical_assertiveness, western_aligned_regimes).
narrative_ontology:constraint_victim(russian_geopolitical_assertiveness, regional_economic_integration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEIGHBORING SOVEREIGN STATE (SNARE) — Trapped by geographic proximity, energy dependency, ethnic/historical ties. Limited alternatives for security alignment or economic partnership. The constraint extracts geopolitical deference, military non-alignment, and de facto spheres of influence. Exit options are minimal — relocation is impossible, economic diversification is blocked by sanctions and energy leverage, and military independence is costly or unachievable. Maximum experienced extraction from a powerless position with trapped exit.
constraint_indexing:constraint_classification(russian_geopolitical_assertiveness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NATO ALLIANCE / WESTERN STATES (TANGLED ROPE) — Organized but constrained by alliance commitments, nuclear parity, and trade interdependencies. The constraint provides genuine coordination function: it defines boundaries of acceptable behavior, establishes deterrence framework, and creates predictability (albeit a predictability of threat). However, it also extracts significant costs: military spending, geopolitical bifurcation, reduced economic integration, and existential risk. Both coordination and asymmetric extraction present simultaneously. Organized actors see exit as costly but possible — they retain economic and military agency.
constraint_indexing:constraint_classification(russian_geopolitical_assertiveness, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: RUSSIAN STATE APPARATUS (ROPE) — Primary beneficiary. Experiences the constraint as coordination mechanism enabling influence projection, security buffer maintenance, and resource extraction. Has arbitrage exit options: can modulate assertiveness levels, shift alliance patterns, engage in diplomatic cycling. Constraint provides pure coordination benefit without experienced extraction — the assertiveness is instrumentally chosen to solve the state's security and influence problems. Net beneficiary position.
constraint_indexing:constraint_classification(russian_geopolitical_assertiveness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL ORDER REFORMERS (SCAFFOLD) — Powerful states seeking to modify the constraint through multipolar frameworks, BRICS integration, or alternative institutional arrangements. See assertiveness as a temporary feature of the current unipolar-to-multipolar transition. Exit pathway exists: if successful integration into alternative institutions occurs, the constraint's coercive mechanism dissolves into negotiated spheres of influence. Sunset clause implicit in the reform agenda. Superpowers with mobile exit options experience lower effective extraction.
constraint_indexing:constraint_classification(russian_geopolitical_assertiveness, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR LEGACY INSTITUTION (PITON) — NATO, nuclear deterrence frameworks, and bloc-based diplomacy persist partly through institutional inertia despite changed strategic environment. Theater ratio (0.55) reflects performative aspects: nuclear signaling, military exercises, diplomatic summits that rehearse Cold War script without achieving new coordination. The institutional machinery maintains the constraint through repetition and threat displays rather than through structural inevitability. Would persist even if geopolitical conditions shifted — institutions don't exit on their own.
constraint_indexing:constraint_classification(russian_geopolitical_assertiveness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REALIST NATURALIZATION (MOUNTAIN) — From a civilizational view, Russian assertiveness appears as an immutable feature of great power competition: geography, power distribution, and security dilemmas create inevitable confrontation. Structural realism frames the constraint as natural law — states with Russia's position will always assert regionally. However, this naturalizes what is contingent: assertiveness requires active enforcement, is modulated by leadership choices, and responds to incentive structures. The mountain classification is a false summit — the analytical observer risks treating institutional choices as laws of nature.
constraint_indexing:constraint_classification(russian_geopolitical_assertiveness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russian_geopolitical_assertiveness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(russian_geopolitical_assertiveness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russian_geopolitical_assertiveness, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(russian_geopolitical_assertiveness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(russian_geopolitical_assertiveness, TR),
    TR >= 0.70.

:- end_tests(russian_geopolitical_assertiveness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Russian state apparatus successfully extracts geopolitical concessions, security buffers, and economic advantages from neighboring states over the 15-year measurement interval. The trajectory from 0.42 to 0.68 shows ratcheting through crisis cycles (Georgia 2008, Ukraine 2014-2022) and failed diplomatic resolution. Extraction is not total (some neighbors maintain limited sovereignty) but substantial and increasing. Suppression (0.72): High. Neighboring states face multiple suppression vectors: geographic inescapability, energy dependency with limited alternatives, military threat credibility backed by demonstrated willingness to use force, and international system indifference (non-intervention by great powers in regional affairs). However, suppression is not absolute — some states (Poland, Baltics) have diversified energy and found NATO protection. The average suppression across all victim states is 0.72. Theater ratio (0.55): Moderate-high. Increasing performative content reflects the constraint's operational mode: military exercises (Zapad), rhetorical escalation, diplomatic ultimatums, and summit cycles that rehearse threat/accommodation patterns without achieving durable resolution. The rise from 0.38 to 0.55 indicates growing theater as assertiveness becomes routinized rather than episodic. Theater is not dominant (would be 0.70+ for piton) but significant and rising.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence exists between beneficiary (institutional/arbitrage, Rope) and primary victim (powerless/trapped, Snare). The beneficiary experiences coordination and influence extension; the victim experiences coercive domination with no exit. NATO occupies an intermediate organized/constrained position experiencing Tangled Rope: genuine alliance coordination for mutual defense mixed with the extraction cost of military spending and geopolitical bifurcation. The multipolar reformers at powerful/mobile experience Scaffold because they retain agency and see exit pathways. The piton perspective reveals degradation: the Cold War institutional machinery persists through inertia (0.55 theater ratio) despite reduced structural necessity. The analytical mountain risks naturalizing what is contingent: geopolitical constraints are enforced through active mechanisms, modulated by leadership choices, and respond to incentive structures — not immutable laws of great power interaction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit option capacity. Russian state apparatus (beneficiary + arbitrage exit) produces d ≈ 0.05, yielding low or negative f(d) → low or negative χ from their perspective (Rope). Trapped neighboring states (victims + trapped exit) produce d ≈ 0.95, yielding high f(d) ≈ 1.42 → high χ experienced as Snare. NATO (mixed beneficiary/victim + constrained exit) produces d ≈ 0.55, yielding f(d) ≈ 0.75 → moderate χ experienced as Tangled Rope. Multipolar reformers (powerful + mobile exit) produce d ≈ 0.40, yielding f(d) ≈ 0.40 → low-moderate χ experienced as Scaffold. Cold War institutions (arbitrage exit despite reduced function) produce d ≈ 0.15, yielding low χ but high theater (Piton). Analytical observer (analytical + analytical exit) produces d ≈ 0.72 but risks naturalizing the structure, generating false mountain. The directionality pipeline differentiates how the same extraction mechanism is experienced differently across positions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's extractiveness (0.68) and suppression (0.72) exceed thresholds for pure snare classification (χ ≥ 0.66 for victims). However, multiple perspectives show genuine coordination functions preventing mislabeling as pure extraction. NATO's Tangled Rope perspective shows that the constraint provides real deterrence coordination (collective defense) alongside extraction costs. The Scaffold perspective shows that alternative multilateral pathways exist with sunset logic. The beneficiary's Rope perspective shows that the assertiveness is instrumentally chosen problem-solving, not gratuitous dominance. The piton perspective shows institutional inertia rather than pure function. The false mountain reveals the risk of naturalizing contingent institutional choices. Mandatrophy is resolved by showing that while the primary victim perspective (Snare) is accurate for trapped neighbors, the complete classification requires all six perspectives. No single type captures the structure. The presheaf of perspectives is the answer, not any individual classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    assertiveness_mechanism_ambiguity,
    'Is Russian assertiveness driven by security dilemma (structural necessity) or by elite preference for spheres of influence (policy choice)?',
    'Counterfactual analysis: modeling different leadership postures; comparison with other rising powers'' behavior; isolation of security requirements from expansionist preference',
    'If security-driven: constraint could be resolved through security guarantees and multilateral frameworks (Scaffold type). If preference-driven: constraint requires containment and counter-balancing (Snare type persists). Classification hinges on the answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assertiveness_mechanism_ambiguity, conceptual, 'Whether assertiveness is structural necessity or elite preference').

omega_variable(
    neighboring_state_coalition_threshold,
    'At what point do trapped neighboring states achieve sufficient coalition density to shift from individual powerlessness to organized collective power?',
    'Empirical monitoring of coordination among neighboring states; tracking of joint security arrangements, economic blocs, multilateral institutions; threshold analysis for coalition effectiveness',
    'If coalition threshold achieved: powerless agents upgrade to organized status; snare classification shifts toward tangled_rope or scaffolding. If coalition remains fragmented: snare persists. Dynamic coalition extension applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neighboring_state_coalition_threshold, empirical, 'Coalition density threshold for neighboring state power aggregation').

omega_variable(
    economic_interdependence_irreversibility,
    'Has economic integration (energy, trade, supply chains) between Russia and Europe reached a level where decoupling permanently increases extraction costs, or are alternative suppliers and pathways feasible?',
    'Energy independence analyses; alternative supply chain modeling; cost-benefit analysis of different decoupling scenarios; timeline projections for alternative infrastructure maturation',
    'If irreversible: suppression persists at high levels (0.72+). If reversible: suppression declines over 10-20 years, potentially enabling Scaffold exit pathway. Affects both extractiveness trajectory and theater ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_interdependence_irreversibility, empirical, 'Whether economic interdependence irreversibly locks in suppression').

omega_variable(
    nuclear_parity_stability,
    'Does nuclear parity genuinely prevent escalation, or does it create fragile deterrence vulnerable to miscalculation and first-strike temptation?',
    'Game-theoretic analysis of escalation pathways; nuclear doctrine comparison; crisis simulation; historical near-miss analysis',
    'If stable: current tangled_rope and snare equilibrium persists indefinitely. If fragile: constraint could collapse into open conflict (mountain shifts to snare or destruction). Risk asymmetry affects classification credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_parity_stability, conceptual, 'Whether nuclear parity provides genuine deterrence stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russian_geopolitical_assertiveness, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rga_tr_t0, russian_geopolitical_assertiveness, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rga_tr_t5, russian_geopolitical_assertiveness, theater_ratio, 5, 0.48).
narrative_ontology:measurement(rga_tr_t10, russian_geopolitical_assertiveness, theater_ratio, 10, 0.55).
narrative_ontology:measurement(rga_tr_t15, russian_geopolitical_assertiveness, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(rga_be_t0, russian_geopolitical_assertiveness, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rga_be_t5, russian_geopolitical_assertiveness, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(rga_be_t10, russian_geopolitical_assertiveness, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(rga_be_t15, russian_geopolitical_assertiveness, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russian_geopolitical_assertiveness, enforcement_mechanism).
narrative_ontology:affects_constraint(russian_geopolitical_assertiveness, ukrainian_sovereignty_extraction).
narrative_ontology:affects_constraint(russian_geopolitical_assertiveness, european_energy_dependence).
narrative_ontology:affects_constraint(russian_geopolitical_assertiveness, nato_alliance_cohesion).
narrative_ontology:affects_constraint(russian_geopolitical_assertiveness, arms_race_spiral).

% DUAL FORMULATION NOTE:
% Russian assertiveness decomposes into distinct constraints: direct territorial extraction (Ukraine), economic leverage (energy), alliance destabilization (NATO cohesion), and arms escalation (nuclear stability). Each has distinct ε values and victim groups. This story captures the umbrella constraint across all domains. Downstream constraints inherit network dependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(russian_geopolitical_assertiveness, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
