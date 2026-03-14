% ============================================================================
% CONSTRAINT STORY: us_hegemonic_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_hegemonic_order, []).

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
 *   constraint_id: us_hegemonic_order
 *   human_readable: US Hegemonic Order
 *   domain: geopolitics/international_relations
 *
 * SUMMARY:
 *   The US hegemonic order represents a global constraint system emerging
 *   from post-WWII power asymmetry and sustained through a combination of
 *   genuine coordination mechanisms (security guarantees, financial system
 *   integration, technology access) and active extraction (seigniorage
 *   privileges, structural adjustment conditionality, sanctions capacity,
 *   military pressure). The constraint exhibits temporal degradation: as peer
 *   competitors accumulate capacity, the coordination component atrophies and
 *   the extraction component becomes more visible. Theater ratio rises as
 *   institutional legitimacy requires increasing performative maintenance
 *   (development rhetoric without development outcomes, multilateral
 *   governance theater without functional decision-making). The constraint
 *   demonstrates all six DR types from different structural positions:
 *   trapped peripheral economies see pure extraction (Snare), regional powers
 *   see mixed coordination-extraction (Tangled Rope), core allies see pure
 *   coordination (Rope), institutional legitimacy apparatus becomes
 *   performative (Piton), emergent coalitions build alternative structures
 *   with explicit sunset (Scaffold), and the US state itself becomes
 *   partially identity-locked to hegemonic maintenance despite increasing
 *   costs (Tangled Rope with identity_locked sublayer). The analytical
 *   observer risks naturalizing the order as an inevitable feature of
 *   international power distribution (Mountain), but structural data reveals
 *   this as a false summit — the order is contingently sustained through
 *   active enforcement that becomes visible when enforcement costs rise.
 *
 * KEY AGENTS:
 *   - Peripheral economies: Primary victim (powerless/trapped) — structurally dependent on dollar trade and capital flows; face sanctions if they attempt alternative arrangements; bear full cost of structural adjustment conditionality
 *   - Regional powers: Secondary victim/partial beneficiary (moderate/constrained) — experience mixed coordination benefits (technology access, security guarantees) and extraction costs (currency volatility, capital flight risk, geopolitical pressure); can organize coalitions but at high cost
 *   - Core allied states: Primary beneficiary (institutional/arbitrage) — integrated security and financial systems provide coordinated benefits; maintain optionality to exit but rarely exercise it; experience the order as pure coordination
 *   - Emerging multipolar coalition: Organized agents (organized/mobile) — building alternative payment systems, development financing, security arrangements; have explicit exit trajectory; constrained but not trapped
 *   - US institutional state: Complex beneficiary-victim (powerful/mobile with identity_locked sublayer) — captures seigniorage and geopolitical leverage but locked into massive military expenditure; has structural capacity to reallocate but demonstrates identity fusion with hegemonic role
 *   - Legacy international institutions: Performative apparatus (institutional/arbitrage) — IMF, World Bank, UN Security Council, NATO increasingly maintain theater rather than function; persist through inertia not necessity
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law of international power distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_hegemonic_order, 0.58).
domain_priors:suppression_score(us_hegemonic_order, 0.65).
domain_priors:theater_ratio(us_hegemonic_order, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_hegemonic_order, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_hegemonic_order, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_hegemonic_order, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_hegemonic_order, tangled_rope).
narrative_ontology:human_readable(us_hegemonic_order, "US Hegemonic Order").
narrative_ontology:topic_domain(us_hegemonic_order, "geopolitics/international_relations").

domain_priors:requires_active_enforcement(us_hegemonic_order).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_hegemonic_order, us_institutional_actors).
narrative_ontology:constraint_beneficiary(us_hegemonic_order, allied_financial_centers).
narrative_ontology:constraint_beneficiary(us_hegemonic_order, dollar_dependent_economies).
narrative_ontology:constraint_victim(us_hegemonic_order, non_aligned_states).
narrative_ontology:constraint_victim(us_hegemonic_order, peripheral_economies).
narrative_ontology:constraint_victim(us_hegemonic_order, global_public_goods_provision).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL ECONOMY (SNARE) — Nations structurally dependent on dollar-denominated trade and capital flows have no viable exit from the hegemonic order. Capital flight, sanctions, and currency devaluation are immediate costs. Suppression mechanisms (structural adjustment conditionality, sanctions regimes, capital controls) eliminate alternatives. The peripheral state perceives maximum extraction with no escape route.
constraint_indexing:constraint_classification(us_hegemonic_order, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL POWER (TANGLED ROPE) — States with regional capacity (India, Brazil, Turkey, Saudi Arabia) experience genuine coordination benefits from the hegemonic order (access to dollar credit, security guarantees, technology transfer) alongside significant extraction. Exit is costly but possible through regional coalition-building and alternative currency arrangements. High suppression but agency exists.
constraint_indexing:constraint_classification(us_hegemonic_order, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CORE ALLIED INSTITUTION (ROPE) — US allies (Western Europe, Japan, South Korea, Australia) perceive the order as pure coordination: common security framework, integrated financial systems, technology access. These institutions have arbitrage optionality — they could theoretically exit, but the cost-benefit calculus is overwhelmingly favorable. Net beneficiaries with agency.
constraint_indexing:constraint_classification(us_hegemonic_order, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL LEGITIMACY APPARATUS (PITON) — The formal institutions of the hegemonic order (IMF, World Bank, UN Security Council, NATO) have become largely performative. Their original coordination functions (post-WWII reconstruction, collective security, financial stability) have atrophied into theater: IMF structural adjustment fails to produce development, UN Security Council is paralyzed, NATO expands beyond collective defense. These institutions persist through inertia, not function. High theater ratio reflects that legitimacy maintenance now exceeds actual governance capacity.
constraint_indexing:constraint_classification(us_hegemonic_order, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EMERGENT MULTIPOLAR COALITION (SCAFFOLD) — Rising powers (China, Russia, India) and their satellite coalitions (BRICS, Shanghai Cooperation Organization, ASEAN regional frameworks) see the hegemonic order as a temporary constraint being systematically dismantled. Alternative payment systems (CIPS, digital yuan), alternative development financing (Belt and Road), and alternative security arrangements are building parallel structures with explicit sunset logic for dollar hegemony. Organized agents with clear exit trajectory.
constraint_indexing:constraint_classification(us_hegemonic_order, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: US INSTITUTIONAL STATE (TANGLED ROPE) — The US state is simultaneously beneficiary and partially victim of the hegemonic order it maintains. Benefits: seigniorage privileges, geopolitical leverage, institutional agenda-setting. Costs: military expenditure requirements (3-4% of GDP), ideological rigidity that prevents strategic pivot, technological competition necessitating constant innovation investment. The US has structural mobility (could reallocate military spending, could tolerate peer competitors) but demonstrates identity_locked exit patterns at the institutional level.
constraint_indexing:constraint_classification(us_hegemonic_order, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: DEVELOPMENT INSTITUTION (PITON) — Official development assistance and development bank structures have become substantially performative. The IMF/World Bank development mandate explicitly conflicts with US geopolitical interests in many regions. Development financing increasingly comes from alternative sources (China, regional banks) while the legacy institutions maintain theater — governance reports, poverty metrics, conditional lending — without producing convergence or structural transformation. Theater ratio has risen as functional capacity has degraded.
constraint_indexing:constraint_classification(us_hegemonic_order, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, hegemonic orders are immutable structural features of international systems with power asymmetries. The hegemonic order 'must' exist when capability distributions are unequal; US hegemony is merely the current instance of an inevitable pattern. However, this perspective naturalizes what is actually a contingent political arrangement sustained by active enforcement and suppression mechanisms. The engine's false summit detector will flag this as a naturalization of the contingent.
constraint_indexing:constraint_classification(us_hegemonic_order, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_hegemonic_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_hegemonic_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_hegemonic_order, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_hegemonic_order, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_hegemonic_order, TR),
    TR >= 0.70.

:- end_tests(us_hegemonic_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The hegemonic order combines genuine coordination benefits (security integration, financial system access, technology transfer for core allies) with significant extraction mechanisms (seigniorage privileges, sanctions capacity, structural adjustment conditionality, military-backed geopolitical pressure). The value reflects that extraction is substantial and systematized but not maximized — peripheral economies could be extracted more completely if the hegemonic power had no interest in long-term stability or development. The upward trajectory (0.42 → 0.58) reflects that extraction becomes more visible and systematic as peer competition forces the hegemonic power to more aggressively defend its privileges. Suppression (0.65): Moderate-high. Multiple suppression mechanisms constrain alternatives: dollar dominance eliminates viable currency options; military positioning prevents regional military independence; sanctions regimes punish non-alignment; conditional lending ties policy autonomy to IMF/World Bank requirements; media dominance (English language, technology platforms, cultural production) makes counter-narratives structurally disadvantaged. These are not total suppressions — alternatives exist and some actors have exercised them (Iran, Russia, DPRK under full sanctions; Venezuela, Cuba through resource scarcity) — but the costs are severe enough that most states rationally accept the order. Theater ratio (0.68): High and rising. Legacy international institutions have become increasingly performative as their functional capacity has atrophied. IMF structural adjustment produces debt servicing not development; UN Security Council is paralyzed; NATO expands beyond collective defense into power projection; World Bank development financing is dwarfed by alternative sources. The legitimacy apparatus requires increasing performative maintenance (summit meetings, development rhetoric, multilateral procedures) while actual function migrates to alternative arrangements (bilateral security guarantees, regional financial systems, de facto great-power condominium). Theater has risen from 0.45 to 0.68 over the interval as the coordination component decayed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across the six types. The peripheral economy (powerless/trapped) sees pure extraction with no exit — classification Snare. The regional power (moderate/constrained) sees mixed coordination and extraction — Tangled Rope. The core ally (institutional/arbitrage) sees pure coordination with optionality — Rope. The legacy institution (institutional/arbitrage) sees itself as degraded ritual — Piton. The emergent coalition (organized/mobile) sees a temporary structure being systematically dismantled — Scaffold. The US state (powerful/mobile but identity_locked) sees itself as beneficiary of coordination it is actively enforcing, despite rising extraction costs — Tangled Rope. The civilizational analytical observer risks seeing an immutable feature of international systems — Mountain. The perspectival gap reveals that the constraint operates through asymmetric structural position, not through objective properties of international relations. Whether the order is 'coordination' or 'extraction' depends entirely on which agent's position you analyze.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Core allies who benefit from the order and have optionality (arbitrage) derive low d values — they experience the order as favorable coordination, not extraction. Regional powers who receive some benefits (technology, security) but face high exit costs (capital flight risk, sanctions threat) derive moderate d values (0.45-0.55) — they experience mixed coordination-extraction. Peripheral economies dependent on dollar trade with no functional exit options derive high d values (0.85-0.95) — they experience maximum extraction regardless of any coordination components. The US state presents the most complex case: as a powerful actor (d ≈ 0.48 structurally), it should experience moderate effective extraction, but identity-fusion with the hegemonic role appears to lock it into defending the order despite rising costs. An identity-locked analytical revision of the US state's perspective would set exit_options to "identity_locked" rather than "mobile", raising the psychological/institutional extraction costs above what the structural derivation alone predicts.
 *
 * MANDATROPHY ANALYSIS:
 *   The US hegemonic order resolves mandatrophy by demonstrating that all six types are legitimate perspectival readings of the same constraint. The mandate is not 'which type is correct?' but 'which perspective captures the agent's structural reality?' The order is genuinely a coordination mechanism for core allies (Rope); genuinely a mixed coordination-extraction system for regional powers (Tangled Rope); genuinely pure extraction for trapped peripheral economies (Snare); genuinely performative ritual for legacy institutions (Piton); genuinely temporary and dismantleable from the multipolar coalition perspective (Scaffold); and genuinely natural from the analytical observer's bird's-eye view (Mountain — false summit). The mandat satisfies itself: the constraint has no single 'true' type; it is a presheaf of types across an indexical space. The rising theater ratio and extractiveness over the interval reflect that as peer competition increases and the coordination component decays, more perspectives shift from coordination-based types (Rope) toward extraction-based or performance-based types (Snare, Piton). This temporal evolution is the constraint's lifecycle: hegemonic orders begin with genuine coordination value (allies benefit) but as competitors accumulate capacity, the extraction component becomes more visible and the coordination rationale more theatrical. The US perspective demonstrates the paradox: the hegemonic power may be most identity-locked to the order even as its objective benefits decline relative to costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dollar_replacement_timeline,
    'What timeline threshold distinguishes structural erosion of dollar hegemony from temporary currency volatility or speculative shifts?',
    'Long-term tracking of reserve currency composition, international settlement patterns, debt denominated in alternative currencies; correlation with geopolitical crisis patterns',
    'If replacement timeline < 10 years: scaffold perspective confirmed, hegemonic order is genuinely degrading. If > 30 years: multipolar coalition view is aspirational; piton perspective dominates (theater persists despite declining function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dollar_replacement_timeline, empirical, 'Timeline for structural replacement of dollar hegemony vs temporary currency shifts').

omega_variable(
    military_capacity_sustainability,
    'Can the US military apparatus sustain hegemonic enforcement across peer competitors without fiscal collapse or institutional burnout?',
    'Historical analysis of defense spending as percentage of GDP; comparative analysis with prior hegemonic powers at similar capability/cost ratios; measurement of military readiness degradation over time',
    'If sustainable: hegemonic order classification stable across longer timescale. If unsustainable (current trajectory): extraction mechanism shifts from external victims to internal fiscal victims; classification shifts toward snare from US institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_capacity_sustainability, empirical, 'Sustainability of military enforcement capability under peer competition').

omega_variable(
    alternative_coordination_sufficiency,
    'Do emerging alternative coordination systems (BRICS payment systems, regional development banks, ASEAN frameworks) provide genuine coordination benefits equal to the legacy hegemonic institutions?',
    'Comparative analysis of financial integration, capital access costs, development outcomes, and trade friction in alternative vs legacy institutional frameworks',
    'If alternatives sufficiently functional: scaffold sunset logic is structural reality. If alternatives remain incomplete or capture-prone: multipolar coalition view is performative, and peripheral economies remain trapped. Determines whether constraint is genuinely transitioning to new configuration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether alternative institutional arrangements provide functional coordination').

omega_variable(
    institutional_identity_lock_us_state,
    'Is US institutional commitment to hegemonic maintenance driven by structural analysis or by identity-fusion with the concept of American exceptionalism and global leadership?',
    'Discourse analysis of policy rationales; identification of counterfactual scenarios where hegemonic commitment would be rationally questioned; measurement of how strongly identity frames override cost-benefit analysis',
    'If identity-locked: US institutional perspective should be reclassified from tangled_rope to identity_locked exit type, indicating the state cannot perceive alternatives even when structurally available. Classification remains tangled_rope but with heightened extraction costs reflecting irrational commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_identity_lock_us_state, conceptual, 'Whether US hegemonic commitment is structural or identity-fused').

omega_variable(
    coordination_vs_extraction_ambiguity,
    'What portion of the hegemonic order''s institutional structure is genuine coordination (public goods provision, security guarantees, financial stability) vs performative theater maintaining extraction flows?',
    'Counterfactual analysis: if extraction mechanisms were removed, how much institutional function would persist? Empirical measurement of development outcomes, security provision, and financial stability under alternative institutional arrangements',
    'Higher coordination proportion → Tangled Rope classification is appropriate with lower extraction component. Higher theater proportion → Piton classification for legacy institutions dominates; extraction component overstated. Affects measurement trajectory and omega resolution priorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ambiguity, empirical, 'Proportion of genuine coordination vs performative theater in hegemonic order institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_hegemonic_order, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ushem_tr_t0, us_hegemonic_order, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ushem_tr_t20, us_hegemonic_order, theater_ratio, 20, 0.62).
narrative_ontology:measurement(ushem_tr_t40, us_hegemonic_order, theater_ratio, 40, 0.68).
narrative_ontology:measurement(ushem_tr_t60, us_hegemonic_order, theater_ratio, 60, 0.72).

% Extraction over time
narrative_ontology:measurement(ushem_be_t0, us_hegemonic_order, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ushem_be_t20, us_hegemonic_order, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ushem_be_t40, us_hegemonic_order, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(ushem_be_t60, us_hegemonic_order, base_extractiveness, 60, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_hegemonic_order, global_infrastructure).
narrative_ontology:affects_constraint(us_hegemonic_order, dollar_reserve_currency_system).
narrative_ontology:affects_constraint(us_hegemonic_order, structural_adjustment_conditionality).
narrative_ontology:affects_constraint(us_hegemonic_order, us_military_basing_network).
narrative_ontology:affects_constraint(us_hegemonic_order, imf_world_bank_governance).
narrative_ontology:affects_constraint(us_hegemonic_order, nato_expansion_dynamics).

% DUAL FORMULATION NOTE:
% The US hegemonic order as global constraint decomposes into multiple structurally distinct component constraints with different ε values: (1) dollar reserve currency system (ε ≈ 0.45, Tangled Rope for peripheral economies) provides coordination for financial centers but extraction through seigniorage; (2) structural adjustment conditionality (ε ≈ 0.72, Snare) pure extraction with minimal coordination function; (3) military basing network (ε ≈ 0.55, Tangled Rope) coordinates security for allies, extracts geopolitical submission from host states; (4) institutional governance structures (ε ≈ 0.40 official claim, 0.65 functional reality, Piton) increasingly theatrical; (5) technology ecosystem integration (ε ≈ 0.35, Rope for allies, 0.50 Tangled Rope for excluded states). Each component has its own beneficiary/victim structure and temporal trajectory. The aggregate constraint story captures the systemic interaction but readers should decompose into component stories for precise analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_hegemonic_order, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
