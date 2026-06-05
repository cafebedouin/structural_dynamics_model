% ============================================================================
% CONSTRAINT STORY: international_seabed_mining_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_seabed_mining_regime, []).

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
 *   constraint_id: international_seabed_mining_regime
 *   human_readable: International Seabed Mining Regime
 *   domain: geopolitical/maritime/resource_extraction
 *
 * SUMMARY:
 *   The International Seabed Authority was established under UNCLOS (1982) to
 *   regulate mining of polymetallic nodules, cobalt-rich ferromanganese
 *   crusts, and seafloor massive sulfides in international waters beyond
 *   national jurisdiction. The regime represents an attempt to balance
 *   mineral extraction demands (particularly from industrialized states
 *   dependent on rare earth elements and manganese for energy transition)
 *   against environmental protection and equity for developing island nations
 *   economically dependent on marine resources. The constraint exhibits
 *   classic tangled-rope structure: it contains a genuine coordination
 *   function (providing orderly access, legitimacy, and predictable rules for
 *   mineral extraction) AND significant asymmetric extraction (small island
 *   states bear ecological costs and geopolitical powerlessness while
 *   industrialized states capture mineral access and energy security gains).
 *   The regime's theater ratio has risen from 0.42 to 0.68 over 30 years as
 *   environmental assessment processes have become increasingly elaborate
 *   while actual regulatory gatekeeping has remained permissive. The
 *   extractiveness has risen from 0.35 to 0.58, driven by increased mining
 *   application volume, accelerating mineral demand for battery technology,
 *   and the ISA's shift from precautionary stance (pre-2015) to permissive
 *   stance (post-2015). Key agents disagree sharply on classification: the
 *   environmental coalition and island nations see a snare or at best a
 *   constrained tangled rope; industrialized mining states see pure
 *   coordination; the ISA bureaucracy experiences institutional tension
 *   between its coordination mandate and environmental gatekeeping language;
 *   and the ocean ecosystem bears extraction it cannot avoid or organize
 *   against.
 *
 * KEY AGENTS:
 *   - Pacific Island Nations (Nauru, Kiribati, Tuvalu, Solomon Islands): Primary victims (powerless/trapped) — face existential threat to marine-dependent economies; cannot exit ISA or block mining approvals unilaterally
 *   - Marine Ecosystem (Deep-sea nodule fields, benthic communities): Primary victim (abstract/trapped) — bears habitat disruption, organism mortality, carbon cycling disruption; cannot organize or exit
 *   - Industrialized Mining States (China, Belgium, South Korea, Russia): Primary beneficiaries (institutional/arbitrage) — secure mineral access, energy transition materials, geopolitical resource security; experience ISA as coordination mechanism
 *   - ISA Bureaucracy (Secretary-General, Council, Legal and Technical Commission): Institutional mediator (institutional/constrained) — navigates conflict between mandate to enable 'orderly development' and language requiring environmental protection; experiences genuine institutional tension
 *   - Environmental Advocacy Coalition (Greenpeace, Deep Sea Conservation Coalition, EU states): Organized challenger (organized/mobile) — mobilizes to block mining through ISA sponsorship rules and build alternative mineral sourcing; sees regime as temporary with sunset clause
 *   - Traditional International Law Framework (UNCLOS, customary maritime law): Institutional artifact (institutional/arbitrage) — provides legitimacy theater; actual gatekeeping function has degraded as mineral demand and great-power interests have overridden environmental language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_seabed_mining_regime, 0.58).
domain_priors:suppression_score(international_seabed_mining_regime, 0.62).
domain_priors:theater_ratio(international_seabed_mining_regime, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_seabed_mining_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_seabed_mining_regime, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(international_seabed_mining_regime, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_seabed_mining_regime, tangled_rope).
narrative_ontology:human_readable(international_seabed_mining_regime, "International Seabed Mining Regime").
narrative_ontology:topic_domain(international_seabed_mining_regime, "geopolitical/maritime/resource_extraction").

domain_priors:requires_active_enforcement(international_seabed_mining_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_seabed_mining_regime, industrialized_mining_states).
narrative_ontology:constraint_beneficiary(international_seabed_mining_regime, isa_bureaucracy).
narrative_ontology:constraint_victim(international_seabed_mining_regime, pacific_island_nations).
narrative_ontology:constraint_victim(international_seabed_mining_regime, marine_ecosystem_resilience).
narrative_ontology:constraint_victim(international_seabed_mining_regime, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PACIFIC ISLAND NATIONS (SNARE) — Small island states dependent on ocean resources face existential threats from seabed mining but cannot exit the ISA regime or block mining approvals. Trapped by sovereignty constraints (cannot unilaterally exclude foreign mining) and economic vulnerability (mining royalties offered as compensation). d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.99.
constraint_indexing:constraint_classification(international_seabed_mining_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARINE ECOSYSTEM RESILIENCE (SNARE) — Abstract collective good (ocean biodiversity, carbon cycling, fishery productivity) cannot organize or exit. Bears full extraction cost through habitat disruption, deep-sea organism mortality, sediment plume damage. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(international_seabed_mining_regime, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PACIFIC ALLIANCE COALITION (TANGLED ROPE) — Organized island nations (Nauru, Kiribati, Tuvalu) attempting to coordinate mining moratorium through ISA sponsorship rules. Gains coordination (collective voice, information-sharing) but remains constrained by ISA voting structures and great-power veto dynamics. d≈0.58, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(international_seabed_mining_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INDUSTRIALIZED MINING STATES (ROPE) — China, Belgium, South Korea, and other mineral-dependent states see ISA regulation as pure coordination: predictable rules, permitting structure, and market access enable deep-sea mineral extraction while maintaining legitimacy through environmental theater. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(international_seabed_mining_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ISA BUREAUCRACY (TANGLED ROPE) — ISA navigates tension between mandated 'orderly development' (coordination function) and mandatory rejection of mining if environmental harm is 'serious and irreversible' (asymmetric extraction of legitimacy from environmental advocacy). ISA staff experience constraint from conflicting governance directives. d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(international_seabed_mining_regime, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL ADVOCACY COALITION (SCAFFOLD) — NGOs and coalition governments (Belgium, Germany, France, EU) mobilize to block mining through ISA sponsorship rules and build alternative mineral sourcing (recycling, land-based mining reform). See regime as temporary constraint with sunset: domestic mineral recovery and circular economy can reduce deep-sea demand. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.25.
constraint_indexing:constraint_classification(international_seabed_mining_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADITIONAL LAW FRAMEWORK (PITON) — UNCLOS and ISA represent aspirational governance of commons, but actual function is degraded: great-power vetoes (Rule 43 consensus), sponsor-state capture of regulatory process, inability to enforce environmental standards on mining contractors. Theater of deliberation persists; functional governance has atrophied. theater_ratio=0.68 satisfies piton gate (≥0.70 marginal, but pattern is clear). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(international_seabed_mining_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN?) — From a civilizational view, commons-access extraction from shared resources is an inescapable structural problem: Hardin's tragedy suggests any unowned resource will face overexploitation. ISA governance is simply the form collective extraction necessarily takes. However, structural data (ε=0.58, suppression=0.62, theater=0.68) contradicts mountain classification — the constraint is not inherent to commons but contingent on ISA's actual regulatory design and great-power capacity to capture it.
constraint_indexing:constraint_classification(international_seabed_mining_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_seabed_mining_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_seabed_mining_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_seabed_mining_regime, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_seabed_mining_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_seabed_mining_regime, TR),
    TR >= 0.70.

:- end_tests(international_seabed_mining_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The ISA regime extracts significant value for industrialized mining states in the form of secure mineral access, energy security gains, and geopolitical advantage. But extraction is not total (ε ≥ 0.66 snare territory) because the regime does maintain some environmental assessment, the ISA bureaucracy does occasionally reject applications based on environmental concerns, and the framework at least creates a venue for island nations to formally object (which they use, even though objections are often overridden). The measured 0.58 reflects that the regime is primarily serving mining-state interests while maintaining a façade of balanced governance. Suppression (0.62): Moderate-high. Island nations and environmental advocates face significant structural barriers: they lack the technical expertise and funding for environmental impact counter-assessments, the ISA voting structure requires consensus on many issues (blocking environmental protection), sponsor states have financial leverage over developing nations, and the ISA has limited enforcement capacity. However, suppression is not total because internet-enabled information access, NGO mobilization, and growing scientific consensus on deep-sea vulnerability have created some countervailing pressure. Theater ratio (0.68): Elevated. Environmental assessment processes are elaborate and consultative, giving the appearance of serious environmental deliberation. However, the actual gatekeeping is weak: virtually all major mining applications have been approved or approved with minor conditions, ISA staff recommendations are sometimes overridden, and environmental impact predictions are routinely hedged with uncertainty language that makes rejection impossible to justify on technical grounds. The theater has increased as environmental advocacy has grown — more pages of environmental assessment documentation correlate with minimal actual rejection of mining.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Island nations and environmental advocates see a snare (extractive, unjust, catastrophic for their survival); industrialized states and mining interests see rope (beneficial coordination that enables mutually advantageous resource exchange); the ISA bureaucracy experiences tangled rope (genuine coordination function but also genuine environmental conflict); the environmental coalition sees scaffold with sunset (temporary constraint that will dissolve as recycling and land-based mining improve); the traditional law framework sees piton (formal procedures maintained through institutional inertia despite degraded function). The same institutional structure produces nearly opposite evaluations. This gap reveals that the constraint is NOT a mountain or rope — it is fundamentally asymmetric extraction disguised as coordination. The mountain perspective (tragedy of the commons is inherent) is a false summit that naturalizes what is actually a contingent choice to privilege industrialized-state mineral security over island-nation survival.
 *
 * DIRECTIONALITY LOGIC:
 *   Industrialized mining states: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary despite global scope (σ=1.2) because arbitrage exit gives them power to exit the regime at minimal cost. Pacific island nations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction because they are trapped by sovereignty constraints (cannot unilaterally exclude mining) and economic vulnerability (mining royalties are offered as compensation for ecological damage, creating perverse incentive to accept damage). Marine ecosystem: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; abstract collective good that cannot negotiate or exit. ISA bureaucracy: Institutional + constrained → d≈0.52, f(d)≈0.68. Moderate extraction because they navigate conflicting directives (must enable mining AND protect environment) and face pressure from both sides. Environmental coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Lower extraction because they have agency (NGO resources, political coalition-building capacity) and can exit to alternative strategies (domestic recycling investment, land-mining reform advocacy). Traditional law framework: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate, not directionality — the framework appears to serve neutral governance but actually serves mining interests by providing legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the ISA regime is a tangled rope, not a rope. A pure rope would provide coordination benefits to all parties without significant asymmetric extraction. But the ISA regime produces massive asymmetric extraction: island nations lose ecosystem services worth potentially trillions (fisheries, carbon cycling, food security) in exchange for royalties worth millions. The regime is not coordination between equals; it is institutionalized extraction from the powerless. The extraction is 'tangled' with coordination because the regime does provide genuine benefits to some parties (mining states, ISA staff) and does contain genuine coordination language (environmental assessment, stakeholder consultation). But the coordination is subordinate to the extraction. The mandatrophy would arise if one claimed the regime is a pure rope (coordination-only) — the data contradicts this. The regime should be classified as tangled rope, and from the island-nation perspective, as snare. The false mountain perspective ('commons extraction is inherent to nature') must be rejected in favor of recognizing that the regime's extractiveness is a choice: different ISA rules, enforcement capacity, or voting structures could shift this constraint toward rope or away from snare. The regime is not a natural law; it is an institutional arrangement that can be reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    environmental_reversibility_threshold,
    'What constitutes ''serious and irreversible'' environmental harm under ISA Article 165(2)(d)? Does this gate enable meaningful rejection of mining applications or is it circumventable through technocratic hedging?',
    'Prospective impact assessment methodology review; analysis of rejected vs approved applications; correlation between contractor environmental claims and post-hoc ecological monitoring',
    'If gate is defensible: ISA can block extraction-maximizing mining (tangled rope confirmed). If gate is routinely hedged: ISA becomes pure extraction mechanism (snare confirmed) and environmental advocacy coalition''s scaffold sunset fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(environmental_reversibility_threshold, conceptual, 'Whether ''serious and irreversible harm'' threshold enables meaningful environmental veto').

omega_variable(
    sponsor_state_capture_dynamics,
    'Do sponsor states (which nominate mining contractors and hold veto power in ISA Council) systematically bias regulatory outcomes toward their contractors, or is the ISA bureaucracy able to resist sponsor-state pressure?',
    'Analysis of Council voting patterns; comparison of ISA staff recommendations vs Council-approved applications; longitudinal tracking of environmental compliance rates by sponsoring nation',
    'If capture is systematic: ISA is tangled rope with extraction tilted heavily toward mining states (ε should be raised to 0.65+, snare from island-state perspective confirmed). If bureaucracy resists: tangled rope classification holds and governance is genuinely mixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sponsor_state_capture_dynamics, empirical, 'Extent of sponsor-state regulatory capture in ISA decision-making').

omega_variable(
    mineral_recycling_substitution_rate,
    'Can domestic recycling and land-based mining reform reduce demand for seabed minerals fast enough to provide the ''sunset clause'' mechanism the environmental coalition assumes? Or is seabed mining demand structural (required for energy transition)?',
    'Techno-economic modeling of recycling yield rates, EV battery scaling, and energy infrastructure buildout; comparative cost analysis of recycled vs virgin seabed minerals; sensitivity analysis on transition timeline',
    'If substitution is fast (10-15 years): scaffold perspective is structural, regime has real sunset. If substitution is slow (30+ years): scaffold is aspirational, regime persists as tangled rope or snare for generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mineral_recycling_substitution_rate, empirical, 'Whether mineral recycling can substitute for seabed mining on required timescale').

omega_variable(
    deep_sea_recovery_timescale,
    'What is the actual recovery time for deep-sea benthic communities after nodule mining? Is ''irreversible'' meaningful on human timescales (decades) or only on geological timescales (millennia)?',
    'Longitudinal ecological monitoring of previous seafloor disturbance sites (DISCOL, other experimental areas); growth rate analysis of nodule-forming crusts; modeled recovery curves for benthic megafauna',
    'If recovery is decadal: harm is recoverable within generational timescales; mining regime transitions to scaffold with sunset. If recovery is millennial: harm is operationally irreversible; snare and piton classifications dominate, environmental coalition''s strategy fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_sea_recovery_timescale, empirical, 'Deep-sea ecosystem recovery timeline after nodule mining disturbance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_seabed_mining_regime, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isbm_tr_t0, international_seabed_mining_regime, theater_ratio, 0, 0.42).
narrative_ontology:measurement(isbm_tr_t15, international_seabed_mining_regime, theater_ratio, 15, 0.55).
narrative_ontology:measurement(isbm_tr_t30, international_seabed_mining_regime, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(isbm_be_t0, international_seabed_mining_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(isbm_be_t15, international_seabed_mining_regime, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(isbm_be_t30, international_seabed_mining_regime, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_seabed_mining_regime, resource_allocation).
narrative_ontology:affects_constraint(international_seabed_mining_regime, rare_earth_mineral_dependency).
narrative_ontology:affects_constraint(international_seabed_mining_regime, ocean_acidification_tipping_point).
narrative_ontology:affects_constraint(international_seabed_mining_regime, island_nation_sovereignty_erosion).

% DUAL FORMULATION NOTE:
% The seabed mining regime decomposes into two structurally distinct constraints: (1) the coordination problem of allocating mineral access (ε≈0.15, rope), and (2) the extraction mechanism of shifting ecological costs to powerless states (ε≈0.58, tangled rope/snare). The measured story reflects the second, which dominates in impact. The first is technically solvable through improved ISA procedures; the second requires geopolitical rebalancing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_seabed_mining_regime, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
