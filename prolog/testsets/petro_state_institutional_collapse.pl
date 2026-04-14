% ============================================================================
% CONSTRAINT STORY: petro_state_institutional_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_petro_state_institutional_collapse, []).

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
 *   constraint_id: petro_state_institutional_collapse
 *   human_readable: Petro-State Institutional Collapse and Resource Curse Lock-In
 *   domain: political_economy/institutional_dynamics
 *
 * SUMMARY:
 *   The petro-state institutional collapse constraint demonstrates how
 *   resource abundance can lock a state into a snare where resource
 *   extraction becomes the primary mechanism of institutional control. Rather
 *   than building diversified institutional capacity, states organized around
 *   hydrocarbon export concentrate authority in extraction monopolies, weaken
 *   tax systems (revenues come from commodity sales, not taxation), and
 *   suppress economic alternatives to maintain control over labor and
 *   capital. The constraint exhibits high suppression (0.72) because
 *   populations lack material alternatives (diversified economies destroyed
 *   or never built), face migration barriers (capital controls, limited visa
 *   access), and experience identity-lock (resource nationalism frames
 *   resource export as patriotic duty or inevitable destiny). The
 *   extractiveness increases over the measurement interval (0.35 → 0.68) as
 *   commodity booms initially fund redistribution but eventually concentrate
 *   in oligarchic hands, while theater increases (0.30 → 0.58) as governance
 *   institutions become performative facades — anti-corruption agencies exist
 *   but don't prosecute oligarchs, parliaments debate but cannot override
 *   executive decree, courts uphold formal rule of law while oligarchic
 *   interests determine substantive outcomes. The constraint is
 *   mandatrophy-resolved: the analytical observer risks treating the resource
 *   curse as an immutable economic law, but the structural analysis reveals
 *   it as a contingent institutional arrangement that depends on ongoing
 *   suppression and oligarchic control. Alternative governance models
 *   (sovereign wealth funds, resource nationalization with independent
 *   boards, institutional investment in non-resource sectors) demonstrate
 *   that resource abundance does not necessitate institutional collapse.
 *
 * KEY AGENTS:
 *   - General Population: Primary victim (powerless/trapped) — faces economic dependency on resource sector, weakened social services, constrained exit via migration or economic alternatives
 *   - Institutional Capacity: Diffuse victim (analytical/trapped) — state institutions systematically degraded to prevent constraint-checking on oligarchic extraction
 *   - Economic Diversification: Structural victim (moderate/trapped) — alternative sectors crowded out by resource-financed state monopolies and oligarchic extraction
 *   - Resource Extraction Oligarchy: Primary beneficiary (institutional/arbitrage) — controls extraction monopolies, benefits from commodity booms, maintains institutional suppression
 *   - Foreign Investors: Secondary beneficiary (institutional/arbitrage) — benefit from low effective taxation, weak labor/environmental enforcement, stable oligarchic governance
 *   - Labor and Reform Coalition: Organized victim-actor (organized/mobile) — organized enough to perceive constraint, mobile enough to threat exit (strikes, protests), but suppressed by state monopoly
 *   - International Governance Institutions: Institutional observer (institutional/constrained) — maintain formal engagement despite institutional decay; theater obscures limited leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(petro_state_institutional_collapse, 0.68).
domain_priors:suppression_score(petro_state_institutional_collapse, 0.72).
domain_priors:theater_ratio(petro_state_institutional_collapse, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(petro_state_institutional_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(petro_state_institutional_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(petro_state_institutional_collapse, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(petro_state_institutional_collapse, snare).
narrative_ontology:human_readable(petro_state_institutional_collapse, "Petro-State Institutional Collapse and Resource Curse Lock-In").
narrative_ontology:topic_domain(petro_state_institutional_collapse, "political_economy/institutional_dynamics").

domain_priors:requires_active_enforcement(petro_state_institutional_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(petro_state_institutional_collapse, resource_extraction_oligarchy).
narrative_ontology:constraint_beneficiary(petro_state_institutional_collapse, foreign_investors).
narrative_ontology:constraint_victim(petro_state_institutional_collapse, general_population).
narrative_ontology:constraint_victim(petro_state_institutional_collapse, institutional_capacity).
narrative_ontology:constraint_victim(petro_state_institutional_collapse, economic_diversification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT POPULATION (SNARE) — Citizens trapped in resource-dependent economy with declining institutional services. No exit via economic diversification (institutions too degraded to support alternative sectors). No exit via emigration (capital controls, limited visa access). Maximum suppression through provision of resource rents that preclude alternative livelihood strategies. The constraint extracts generational commitment to resource extraction pathway.
constraint_indexing:constraint_classification(petro_state_institutional_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-SECTOR ENTERPRISE (SNARE) — Small and medium businesses face constrained exit: resource revenues subsidize inefficient state monopolies, crowding out private sector development. High barriers to entry in non-resource sectors (weak IP protection, unstable rule of law, capital costs). Corruption and extraction via informal taxation (protection payments, licensing denial, labor coercion) necessary to operate. Constrained rather than trapped — firms can exit to diaspora or informality, but at high cost.
constraint_indexing:constraint_classification(petro_state_institutional_collapse, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXTRACTION OLIGARCHY (ROPE) — Sees the constraint as coordination: stabilizing commodity prices, managing extraction schedules, controlling labor supply through state monopoly. Experiences net benefit (arbitrage) from the institutional arrangement — they ARE the arrangement. High exit optionality (capital mobility, dual citizenship, foreign accounts). Low effective extraction experienced because they control the mechanism. The constraint enables their power; they do not perceive themselves as constrained by it.
constraint_indexing:constraint_classification(petro_state_institutional_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOREIGN INVESTORS (ROPE) — View the petro-state as a coordination problem (securing stable concessions, managing sovereign risk, coordinating with local oligarchy). High exit optionality (portfolio rebalancing, relocate operations, exit country). Benefit from low effective taxation, weak labor/environmental enforcement, and oligarchic stability. The constraint coordinates their interests with local elites; experienced as pure coordination, not extraction.
constraint_indexing:constraint_classification(petro_state_institutional_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR AND REFORM COALITION (TANGLED ROPE) — Organized agents (unions, NGOs, opposition parties) face mixed extraction and coordination. The constraint both enables their organization (distributed resource rents create a constituency for redistribution) and suppresses their power (state monopoly on enforcement, fragmentation incentives, surveillance and arrest risk). They have some exit optionality (international support, diaspora networks) but operate under high suppression. Mixed experience of the constraint.
constraint_indexing:constraint_classification(petro_state_institutional_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INT'L GOVERNANCE (PITON) — IMF, World Bank, UN, regional bodies maintain institutional relationships with petro-states despite institutional decay and human rights failures. Engagement is substantially performative (conditional loans, governance covenants, technical assistance) with minimal enforcement mechanism against noncompliance. The institutions persist in their engagement through inertia and treaty obligations, not because the mechanism functions. Theater ratio reflects the gap between governance conditionality rhetoric and compliance reality.
constraint_indexing:constraint_classification(petro_state_institutional_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN?) — Risk of naturalizing the resource curse as an immutable economic law: countries with resource abundance inevitably experience institutional decay, rentier states, and authoritarian consolidation. This perspective misses that the resource curse is a contingent institutional arrangement, not a law of nature. The false summit reveals that analysts often treat path dependency as necessity.
constraint_indexing:constraint_classification(petro_state_institutional_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(petro_state_institutional_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(petro_state_institutional_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(petro_state_institutional_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(petro_state_institutional_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(petro_state_institutional_collapse, TR),
    TR >= 0.70.

:- end_tests(petro_state_institutional_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68, high): Resource extraction oligarchs capture commodity rents that would otherwise fund public institutions, social services, or diversified development. The extraction is severe because it locks the state into resource dependency, preventing institutional capacity development. The value reflects that extraction accumulates over time as booms concentrate wealth while busts prevent diversification. Suppression (0.72, very high): Populations face multiple binding constraints: economic (no livelihood alternatives due to institutional degradation), legal (capital controls, labor law restrictions favoring resource sector), and cognitive (identity fusion with resource nationalism). Escape is materially difficult and psychologically difficult. Theater ratio (0.58, moderate-high): Governance institutions perform formal functions (courts, legislatures, agencies) without substantive constraint on extraction. Conditionality from international actors is rhetorical — noncompliance carries no effective sanctions.
 *
 * PERSPECTIVAL GAP:
 *   The oligarchy experiences the constraint as pure coordination (Rope) — managing extraction, labor, and international partnerships. The foreign investors experience it the same way (Rope) — securing concessions and managing political risk. But the dependent population experiences it as maximum extraction with no exit (Snare) — resource dependency locks them into structures they did not choose and cannot escape. The reform coalition perceives mixed constraint (Tangled Rope) — the system both enables their organization (constituencies demanding redistribution) and suppresses their power (arrest, coercion, fragmentation). International institutions perform engagement while having no real leverage (Piton). The analytical observer risks seeing the resource curse as a mountain — immutable economic law — but the structural data reveals this as false naturalization: alternative governance models exist, and institutional reform is possible despite the difficulty.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation reveals why this constraint is robustly snare across multiple perspectives. For the powerless population, d ≈ 0.92 (trapped exit + victim status) produces maximum f(d) ≈ 1.42, generating high experienced chi. For the institutional oligarchy, d ≈ 0.05 (beneficiary status + arbitrage exit) produces negative f(d) ≈ -0.12, generating negative chi (they experience being subsidized by the constraint). For the reform coalition, d ≈ 0.65 (mixed victim/beneficiary, constrained/mobile exit) produces moderate f(d) ≈ 1.00, generating moderate chi. The divergence between the powerless (high d, high chi) and the oligarchy (low d, negative chi) is the core of the snare structure. Foreign investors experience similar low d because they have arbitrage exit options, making the constraint appear purely coordinating from their perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH MANDATE BURDEN: The petro-state exhibits mandate creep where initial extraction (funding state services through resource monopoly) has become the primary mechanism of institutional control. The analytical observer perceives this as resource curse (mountain), naturalizing the arrangement. But the mandate — using resource wealth to fund public goods — is distinct from the extraction mechanism. Alternative mandates exist: resource wealth could fund diversification, institutional development, or equitable distribution. The fact that it instead funds oligarchic consolidation and institutional degradation reflects governance choices, not economic necessity. The mandatrophy is resolved by distinguishing the coordinate problem (how to manage resource wealth) from the extractive mechanism (how to suppress institutional alternatives and concentrate control). The constraint is structurally a snare (high extraction, high suppression, no meaningful exit), and mandatrophy is resolved by recognizing that the mandate (resource management) does not justify the extraction level or suppression intensity. Reform would require either: (1) institutional transformation to constrain extraction (create independent resource boards, strengthen parliament, protect civil society), or (2) commodity diversification that reduces resource dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_curse_mechanism_vs_governance_failure,
    'Is institutional collapse driven by resource abundance itself (curse mechanism: fungibility, low tax requirements, rent-seeking incentives) or by governance failure (weakness of accountability institutions, regulatory capture, weak state capacity)?',
    'Comparative case analysis: resource-rich states with functional institutions (Norway, Botswana, Chile) vs non-resource states with similar governance failures. Identify whether resource abundance is sufficient for collapse or merely enabling.',
    'If curse mechanism: the petro-state is trapped by structural economics (mountain-like). If governance failure: institutional reform is possible (snare not immutable). Classification remains snare either way, but the exit pathway differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_curse_mechanism_vs_governance_failure, empirical, 'Whether collapse is due to resource curse or governance failure').

omega_variable(
    oligarch_preference_for_institutional_collapse,
    'Do oligarchs actively prefer weak institutions (easier rent extraction, fewer constraints on coercion) or are weak institutions a side effect they tolerate for other reasons?',
    'Historical analysis of oligarch political choices: support for rule of law strengthening initiatives, anti-corruption campaigns, or institutional investment. Compare rhetoric vs resource allocation.',
    'If active preference: suppression is intentional (snare classification robust). If tolerated side-effect: there may be defection pathways for oligarchs toward institutional strengthening (converting snare to tangled rope). Affects whether exit is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligarch_preference_for_institutional_collapse, empirical, 'Whether oligarchs actively prefer or passively tolerate institutional weakness').

omega_variable(
    commodity_price_volatility_as_binding_mechanism,
    'Is the lock-in mechanism the commodity price cycle itself (boom-collapse-austerity cycles that prevent institution-building) or the institutional structures built during booms (that collapse when prices fall)?',
    'Time-series analysis of institutional investment correlations with commodity prices. Identify whether reform efforts occur during booms or busts, and whether boom-time investments create durable institutions.',
    'If price volatility is primary: the constraint may have natural sunset (when commodity demand permanently declines or prices stabilize). If institutional structures are primary: sunset depends on deliberate reform. Affects scaffold classification prospect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commodity_price_volatility_as_binding_mechanism, empirical, 'Whether commodity volatility or institutional path dependency is primary binding mechanism').

omega_variable(
    identity_fusion_vs_structural_mobility,
    'To what degree do citizens'' identity-locked commitment to resource nationalism prevent exit, vs structural economic barriers trap them?',
    'Survey and ethnographic analysis of emigration preferences and actual emigration rates. Distinguish between identity-lock (internal inability to imagine exit despite structural mobility) and structural trapping (material barriers).',
    'If identity_locked dominant: cognitive reframing (decolonial nationalism, alternative identity frameworks) could enable exit perception. If trapped dominant: material barriers (visa access, capital controls) must be removed. Affects whether classification from powerless perspective is truly snare or partially identity_locked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_vs_structural_mobility, empirical, 'Degree to which citizens are identity-locked vs structurally trapped in resource dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(petro_state_institutional_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(petro_tr_t0, petro_state_institutional_collapse, theater_ratio, 0, 0.3).
narrative_ontology:measurement(petro_tr_t10, petro_state_institutional_collapse, theater_ratio, 10, 0.45).
narrative_ontology:measurement(petro_tr_t20, petro_state_institutional_collapse, theater_ratio, 20, 0.58).
narrative_ontology:measurement(petro_tr_t5, petro_state_institutional_collapse, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(petro_be_t0, petro_state_institutional_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(petro_be_t10, petro_state_institutional_collapse, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(petro_be_t20, petro_state_institutional_collapse, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(petro_be_t5, petro_state_institutional_collapse, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(petro_state_institutional_collapse, resource_allocation).
narrative_ontology:boltzmann_floor_override(petro_state_institutional_collapse, 0.18).
narrative_ontology:affects_constraint(petro_state_institutional_collapse, commodity_price_volatility).
narrative_ontology:affects_constraint(petro_state_institutional_collapse, capital_flight_and_brain_drain).
narrative_ontology:affects_constraint(petro_state_institutional_collapse, authoritarian_consolidation_through_resource_control).
narrative_ontology:affects_constraint(petro_state_institutional_collapse, environmental_degradation_from_extraction).

% DUAL FORMULATION NOTE:
% The petro-state collapse is downstream of commodity market structures and upstream of authoritarian consolidation. Resource extraction oligarchies maintain power through control of extraction revenue, which enables suppression of institutional alternatives. The constraint family includes commodity volatility (which drives boom-bust cycles enabling extraction), capital flight (escape mechanism for oligarchs but not populations), authoritarian consolidation (institutional suppression enabling continued extraction), and environmental degradation (second-order consequence of unregulated extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(petro_state_institutional_collapse, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
