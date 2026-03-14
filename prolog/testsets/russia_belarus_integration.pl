% ============================================================================
% CONSTRAINT STORY: russia_belarus_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russia_belarus_integration, []).

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
 *   constraint_id: russia_belarus_integration
 *   human_readable: Russia-Belarus Integration Constraint
 *   domain: geopolitical/institutional
 *
 * SUMMARY:
 *   The Russia-Belarus integration constraint represents a geopolitically
 *   significant extraction mechanism operating through institutional forms
 *   that claim coordination as their primary function. The Union State Treaty
 *   (1997, deepened 2019-2022) creates a formal framework for progressive
 *   political, military, and economic integration that extracts Belarusian
 *   sovereignty while delivering genuine security coordination benefits. The
 *   constraint's structure is characterized by asymmetric information,
 *   conditional enforcement, and layered institutional opacity. Theater ratio
 *   has increased from 0.45 (1997-2005, period of genuine negotiation with
 *   some independent Belarusian input) to 0.62 (2022-present, period of
 *   Russian security apparatus dominance post-Ukraine invasion), reflecting
 *   the substitution of negotiation theater for direct enforcement.
 *   Extractiveness has more than doubled from 0.35 (early union period with
 *   some economic benefit to Belarus) to 0.58 (current period where energy
 *   pricing is overtly politicized and military integration serves Russian
 *   geostrategic interests). The constraint exhibits all six DR types
 *   depending on observer position, making it a diagnostic exemplar of how
 *   the same structural mechanism appears as natural law, coordination,
 *   temporary problem, degraded ritual, mixed hybrid, or pure extraction from
 *   different perspectives.
 *
 * KEY AGENTS:
 *   - Russian Executive: Primary beneficiary (institutional/arbitrage) — extends sphere of influence, consolidates energy leverage, prevents NATO expansion in buffer zone
 *   - Belarusian Economic Sovereignty: Primary victim (powerless/trapped) — energy pricing conditionality, export market dependency, currency coordination constrains independent monetary policy
 *   - Belarusian Political Leadership: Secondary actor (powerful/constrained) — trapped between security dependency on Russia and domestic pressure for independence; forced to coordinate while sovereignty erodes
 *   - Belarusian Security Apparatus: Secondary beneficiary (institutional/constrained) — gains from integrated command structure and Russian military support, but constrained by Russian operational dominance
 *   - Integration Institutional Apparatus (Union State Council): Theatrical institution (institutional/constrained) — maintains appearance of negotiation while key decisions follow security apparatus logic
 *   - Alternative Pathway Coalition (EU/NATO): Organized external constraint (organized/mobile) — provides structural exit option if geopolitical conditions shift
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent extraction as geographic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russia_belarus_integration, 0.58).
domain_priors:suppression_score(russia_belarus_integration, 0.65).
domain_priors:theater_ratio(russia_belarus_integration, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russia_belarus_integration, extractiveness, 0.58).
narrative_ontology:constraint_metric(russia_belarus_integration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(russia_belarus_integration, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russia_belarus_integration, tangled_rope).
narrative_ontology:human_readable(russia_belarus_integration, "Russia-Belarus Integration Constraint").
narrative_ontology:topic_domain(russia_belarus_integration, "geopolitical/institutional").

domain_priors:requires_active_enforcement(russia_belarus_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(russia_belarus_integration, russian_executive).
narrative_ontology:constraint_beneficiary(russia_belarus_integration, belarusian_security_apparatus).
narrative_ontology:constraint_victim(russia_belarus_integration, belarusian_economic_sovereignty).
narrative_ontology:constraint_victim(russia_belarus_integration, belarusian_political_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BELARUSIAN ECONOMIC SOVEREIGNTY (SNARE) — Trapped within energy dependency and customs union without structural exit option. Bears full cost of integration: energy pricing tied to political compliance, export markets conditioned on integration deepening, currency coordination constrains monetary policy. Minimal coordination function perceived; maximum extraction experienced.
constraint_indexing:constraint_classification(russia_belarus_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BELARUSIAN POLITICAL LEADERSHIP (TANGLED ROPE) — Constrained by security dependency on Russian military and intelligence apparatus, but also coordinates on shared security interests against NATO expansion and internal stability. Genuine coordination function (unified defense posture) alongside asymmetric extraction (sovereignty erosion, policy conditionality). Active enforcement required: military bases, shared security operations, political surveillance integration.
constraint_indexing:constraint_classification(russia_belarus_integration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RUSSIAN EXECUTIVE (ROPE) — Net beneficiary experiencing the constraint as pure coordination: union deepening enables geopolitical positioning against NATO, consolidates energy leverage, extends sphere of influence. Extraction runs toward this agent; experiences the mechanism as solving the coordination problem of regional dominance.
constraint_indexing:constraint_classification(russia_belarus_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: INTEGRATION INSTITUTIONAL APPARATUS (PITON) — The Supreme State Council and various bilateral commissions are largely theatrical: they produce documents (Union State Treaty, integration roadmaps) with low functional impact. Theater ratio high (0.62) because the institutions maintain the appearance of negotiation and coordination while key decisions are made through security apparatus channels and bilateral pressure. The apparatus persists through inertia — it legitimizes the constraint rather than executing genuine coordination.
constraint_indexing:constraint_classification(russia_belarus_integration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ALTERNATIVE INTEGRATION PATHWAY COALITION (SCAFFOLD) — EU and NATO membership pathways represent a sunset clause on Russian-dominated integration. Poland's EU membership provided alternative economic orbit; Baltic states' NATO membership provided security alternative. For Belarus, these pathways are currently constrained by geography and power asymmetry, but structural availability creates exit option. If EU sanctions on Belarus were lifted and NATO expanded membership (low probability, high cost), the constraint's enforcement mechanism would collapse. Sunset visible but not immediate.
constraint_indexing:constraint_classification(russia_belarus_integration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOGRAPHIC DETERMINISM (MOUNTAIN) — From a civilizational scope, some geopolitical integration between contiguous states is treated as inevitable: geographic proximity, shared history, power asymmetry create an immutable constraint on Belarusian autonomy. This perspective naturalizes contingent extraction as geographic law. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that geographic determinism is a framing device that naturalizes political choice.
constraint_indexing:constraint_classification(russia_belarus_integration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russia_belarus_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(russia_belarus_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russia_belarus_integration, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(russia_belarus_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(russia_belarus_integration, TR),
    TR >= 0.70.

:- end_tests(russia_belarus_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from Belarus through energy pricing mechanisms ($3-5bn annual implicit transfer), policy conditionality (alignment on geopolitics, domestic security), and sovereignty erosion (military integration, customs union, currency coordination). However, extractiveness is not as severe as a pure Snare (0.70+) because Belarus receives genuine security coordination benefits: protection against external aggression, unified command structure against NATO expansion, intelligence sharing. The extraction is embedded within authentic coordination. Measurement trajectory shows acceleration 2014-2022 (Ukraine invasion period): extractiveness jumped from 0.42 (2014) to 0.58 (2022) as Russian security apparatus replaced negotiation with enforcement. Suppression (0.65): High. Barriers to Belarusian exit include: energy dependency (60% of crude oil from Russia), military vulnerability (NATO proximity, shared borders), domestic regime stability dependent on Russian security apparatus, geopolitical isolation if exiting. These are real structural barriers, not merely internalized constraints. But suppression is not total (0.85+) because alternative pathways exist (EU sanctions removal, Baltic model adoption) — they are politically costly but not physically impossible. Theater ratio (0.62): Moderate-high. The Union State Treaty institutions (Supreme State Council, numerous bilateral commissions) produce integration documents and coordination statements with low functional impact on policy decisions. Key decisions (energy pricing, military basing, intelligence sharing) follow bilateral pressure and security apparatus logic, not institutional process. Theater has increased as Russian dominance has hardened: early period (1997-2005) featured genuine negotiation; current period (2022+) features institutional ratification of security-apparatus decisions.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap separates the Russian beneficiary's perception of pure coordination from the Belarusian victim's perception of pure extraction. From the Russian executive position (institutional/arbitrage), the constraint solves a genuine coordination problem: consolidating geopolitical position, preventing NATO encirclement, maintaining sphere of influence. From the Belarusian independence position (powerless/trapped), the same mechanism extracts sovereignty: energy pricing as leverage, political conditionality, military dominance. The Belarusian political leadership sees a hybrid (Tangled Rope) — they are trapped between security dependency and sovereignty erosion, genuinely coordinating on security while experiencing asymmetric extraction of political control. The integration apparatus sees its own theatrical nature (Piton) — it performs coordination while enforcement happens through security channels. The alternative pathway coalition sees a temporary problem with a sunset (Scaffold) — EU and NATO membership would collapse the constraint if geopolitical conditions allowed. The civilizational analyst risks the false summit (Mountain) — treating geopolitical extraction as inevitable geographic law rather than contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's beneficiary/victim status plus exit options. Russian executive: beneficiary + arbitrage exit → d ≈ 0.08 → f(d) ≈ -0.12 (negative effective extraction — agent is subsidized by the constraint). Belarusian independence: victim + trapped exit → d ≈ 0.92 → f(d) ≈ 1.38 (maximum experienced extraction). Belarusian political leadership: both victim (sovereignty erosion) and beneficiary (security coordination) + constrained exit → d ≈ 0.65 → f(d) ≈ 0.95 (moderate extraction; the perspectival gap is real). The pipeline computes chi from ε × f(d) × σ(S) with continental scope (σ=1.1): Russian perspective gets χ ≈ 0.58 × (-0.12) × 1.1 ≈ -0.08 (perceived as coordination benefit); Belarusian victim gets χ ≈ 0.58 × 1.38 × 1.1 ≈ 0.88 (perceived as severe extraction, pushing toward Snare); Belarusian leadership gets χ ≈ 0.58 × 0.95 × 1.1 ≈ 0.60 (perceived as mixed). These computed chi values drive the classification differences.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY PERSPECTIVAL PLURALITY: The mandatrophy tension ('Is this coordination or extraction?') is resolved by showing that both are true from different structural positions. The constraint IS both: Russia achieves genuine geopolitical coordination; Belarus suffers genuine sovereignty extraction. The Tangled Rope classification is correct because: (1) coordination function exists and is real (shared security posture, unified command structure, intelligence integration); (2) asymmetric extraction exists and is severe (energy pricing conditionality, policy constraints, sovereignty erosion); (3) active enforcement is required (military bases, intelligence apparatus integration, political conditionality); (4) both beneficiaries and victims are present and identifiable. No single type resolves the tension — the presheaf over perspectives does. The false summit (Mountain from civilizational analyst) reveals that geographic determinism is a framing device, not a law of nature. The Snare (victim perspective) reveals the extraction magnitude. The Rope (beneficiary perspective) reveals the coordination reality. The Tangled Rope (leadership perspective) reveals the genuinely mixed experience. Together they produce mandatrophy resolution through structural plurality rather than definitional ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_security_coordination,
    'What proportion of the Russian-Belarusian integration mechanism is genuine security coordination against external threat vs. extraction of political control?',
    'Comparative analysis with other regional security arrangements (NATO, Shanghai Cooperation Organization). Assessment of whether security benefits flow symmetrically or concentrate with Russian capability gains. Historical counterfactual: what would Belarusian security posture be under alternative partnerships?',
    'If coordination is genuine: constraint classifies as Rope from both perspectives. If coordination is a cover story: constraint classifies as Snare from Belarusian perspective and Rope from Russian perspective (asymmetric extraction). Current classification assumes asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_security_coordination, empirical, 'Proportion of legitimate security coordination vs. extraction mechanism').

omega_variable(
    exit_option_structural_vs_political,
    'Is Belarusian exit from integration structurally impossible (trapped) or politically forbidden (identity_locked)?',
    'Analysis of what barriers are material (energy cutoff, military invasion capacity, geographic vulnerability) vs. internalized (Belarusian security identity fused with Russian partnership, state legitimacy dependent on union narrative). Counterfactual: what would happen if Belarus attempted EU integration? Military risk vs. reputational/identity cost.',
    'If structurally trapped: classification remains Snare for victim perspective. If identity_locked: classification becomes Rope at biographical horizon (victim perceives mutability even if exits are costly). Current classification uses trapped assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_structural_vs_political, empirical, 'Whether Belarusian exit barriers are structural or internalized').

omega_variable(
    extraction_magnitude_energy_vs_sovereignty,
    'Does measured extractiveness (0.58) adequately capture the magnitude of sovereignty extraction, or does the energy dependency measurement dominate and obscure political extraction?',
    'Decomposition into separate constraint stories: (a) energy dependency mechanism (ε ≈ 0.52); (b) political sovereignty erosion (ε ≈ 0.68). Check whether unified measurement at 0.58 masks distributional asymmetry.',
    'If decomposed: victim perspective (Belarusian political independence) should show higher ε (0.68, moving toward Snare rather than Tangled Rope). Unification may underestimate extraction severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_magnitude_energy_vs_sovereignty, empirical, 'Whether extractiveness measurement obscures asymmetric extraction mechanisms').

omega_variable(
    mandatrophy_resolution_path,
    'How does the classification resolve the tension between ''coordination that enables security cooperation'' and ''extraction that erodes sovereignty''?',
    'Multi-perspective analysis (current approach) showing that both claims are true from different structural positions. Russian executive sees genuine coordination; Belarusian independence sees extraction. Tangled Rope correctly classifies the hybrid because it requires BOTH coordination function AND asymmetric extraction at gate.',
    'Mandatrophy is resolved by perspectival plurality: the constraint IS both coordination and extraction; the classification type depends on observer position. No single type resolves the tension — the presheaf over perspectives does.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_resolution_path, conceptual, 'How Tangled Rope classification resolves coordination vs extraction tension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russia_belarus_integration, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbi_tr_t0, russia_belarus_integration, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rbi_tr_t8, russia_belarus_integration, theater_ratio, 8, 0.54).
narrative_ontology:measurement(rbi_tr_t16, russia_belarus_integration, theater_ratio, 16, 0.62).

% Extraction over time
narrative_ontology:measurement(rbi_be_t0, russia_belarus_integration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rbi_be_t8, russia_belarus_integration, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(rbi_be_t16, russia_belarus_integration, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russia_belarus_integration, enforcement_mechanism).
narrative_ontology:affects_constraint(russia_belarus_integration, nato_expansion_containment).
narrative_ontology:affects_constraint(russia_belarus_integration, belarusian_domestic_security_apparatus).
narrative_ontology:affects_constraint(russia_belarus_integration, russian_energy_leverage_system).

% DUAL FORMULATION NOTE:
% Russia-Belarus integration should be decomposed into at least two structurally distinct constraint stories: (a) energy_dependency_mechanism (ε ≈ 0.52, Resource Allocation type); (b) political_sovereignty_integration (ε ≈ 0.68, Enforcement Mechanism type). The unified story at ε=0.58 represents the composite, but decomposition reveals that extractiveness is asymmetrically distributed: energy extraction is moderate and has legitimate coordination function; political extraction is severe and has minimal coordination benefit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(russia_belarus_integration, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
