% ============================================================================
% CONSTRAINT STORY: sotu_1988_reagan_limited_government_personal_liberty_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1988_reagan_limited_government_personal_liberty_doctrine, []).

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
 *   constraint_id: sotu_1988_reagan_limited_government_personal_liberty_doctrine
 *   human_readable: Limited Government & Personal Liberty Doctrine (Reagan 1988 SOTU)
 *   domain: governance/constitutional_philosophy
 *
 * SUMMARY:
 *   The limited government doctrine articulated in Reagan's 1988 State of the
 *   Union constrains federal authority scope to preserve 'personal liberty
 *   and market-driven prosperity.' This doctrine institutionalizes a
 *   structural bias toward decentralized decision-making and market
 *   mechanisms over federal command, claiming to recover Founding Fathers'
 *   intent regarding constitutional limits on federal power. Benefits accrue
 *   to individuals and regions with capital and entrepreneurial capacity to
 *   self-organize outside federal coordination; costs fall on populations
 *   dependent on federal redistributive (welfare, unemployment, healthcare
 *   access) or regulatory (environmental, labor safety, consumer protection)
 *   protections. The doctrine functions as an overarching ideological
 *   constraint on the scope of legitimate federal action, enforced through
 *   constitutional interpretation, political rhetoric, and court decisions.
 *   The constraint's theater_ratio (0.68) reflects the gap between the
 *   doctrine's rhetorical centrality and its actual functional impact on
 *   day-to-day governance: federal scope remains vast (commerce clause,
 *   spending authority, regulatory agencies all expanded under Reagan), yet
 *   the doctrine continues to delegitimize expansive federal action in
 *   discourse even as institutional practice expands it. Base extractiveness
 *   has risen from 0.42 (1980, pre-Reagan) to 0.64 (1992, post-Reagan),
 *   reflecting cumulative policy effects: welfare caseloads concentrated
 *   among dependent populations, environmental and labor protections weakened
 *   for regions dependent on federal enforcement, and state governments
 *   facing competitive pressure to lower standards.
 *
 * KEY AGENTS:
 *   - Capital Holders & Entrepreneurial Class (institutional/arbitrage) — Primary beneficiary; doctrine constrains taxation, regulation, and redistribution precisely where it would limit capital mobility and wealth accumulation
 *   - Federal Redistributive Dependent Populations (powerless/trapped) — Primary victim; narrow in federal scope removes protections where exit options are most limited; suppression is acute because reframing federal protection as 'overreach' delegitimizes the protective mechanism itself
 *   - Regulatory Protection Beneficiaries (powerless/trapped) — Workers in hazardous industries, pollution-adjacent communities; federal regulatory authority (OSHA, EPA, FDA) becomes constrained where market mechanisms fail; regional alternatives inadequate due to competitive pressure
 *   - Middle-Class Earners (moderate/constrained) — Experience genuine coordination benefit (property rights, contract enforcement) alongside extraction (taxes fund redistribution they oppose); can exit partially through political voice and capital flight to favorable jurisdictions
 *   - State & Regional Governments (organized/constrained) — Gain autonomy from federal constraint but face race-to-bottom competitive pressure; cannot enforce redistribution or environmental protection without federal cooperation
 *   - Constitutional Originalist Interpretive Community (institutional/arbitrage) — Maintains doctrine through performative constitutional interpretation; scholarship and court opinions provide cover for ideological preference; function degraded (courts authorize vast federal powers while maintaining originalist rhetoric)
 *   - Analytical Observer (analytical/analytical) — Risks naturalizing contingent institutional arrangement (scope constraint) as natural law of political economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1988_reagan_limited_government_personal_liberty_doctrine, 0.58).
domain_priors:suppression_score(sotu_1988_reagan_limited_government_personal_liberty_doctrine, 0.62).
domain_priors:theater_ratio(sotu_1988_reagan_limited_government_personal_liberty_doctrine, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1988_reagan_limited_government_personal_liberty_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1988_reagan_limited_government_personal_liberty_doctrine, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1988_reagan_limited_government_personal_liberty_doctrine, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1988_reagan_limited_government_personal_liberty_doctrine, tangled_rope).
narrative_ontology:human_readable(sotu_1988_reagan_limited_government_personal_liberty_doctrine, "Limited Government & Personal Liberty Doctrine (Reagan 1988 SOTU)").
narrative_ontology:topic_domain(sotu_1988_reagan_limited_government_personal_liberty_doctrine, "governance/constitutional_philosophy").

domain_priors:requires_active_enforcement(sotu_1988_reagan_limited_government_personal_liberty_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1988_reagan_limited_government_personal_liberty_doctrine, capital_holders).
narrative_ontology:constraint_beneficiary(sotu_1988_reagan_limited_government_personal_liberty_doctrine, regional_autonomy_advocates).
narrative_ontology:constraint_beneficiary(sotu_1988_reagan_limited_government_personal_liberty_doctrine, market_mechanism_constituencies).
narrative_ontology:constraint_victim(sotu_1988_reagan_limited_government_personal_liberty_doctrine, federal_redistributive_dependent_populations).
narrative_ontology:constraint_victim(sotu_1988_reagan_limited_government_personal_liberty_doctrine, regulatory_protection_beneficiaries).
narrative_ontology:constraint_victim(sotu_1988_reagan_limited_government_personal_liberty_doctrine, equalization_enforcement_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT POPULATION (SNARE) — Low-income, disabled, elderly, and unemployed populations dependent on federal redistributive programs face maximum extraction. The constraint narrows federal scope precisely where their exit options are most limited. Cannot relocate to alternative governance jurisdictions; cannot self-organize market solutions at scale they need. Doctrine creates suppression by redefining dependence as moral failure rather than structural need, making exit (independence) ideologically mandatory while materially impossible. No beneficiary status, no arbitrage path.
constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATORY PROTECTION BENEFICIARIES (SNARE) — Workers in hazardous industries, communities adjacent to pollution sources, consumers without technical capacity for individual quality verification face high extraction under scope restriction. Federal regulatory authority (OSHA, EPA, FDA) becomes constrained precisely where market mechanisms fail. Suppression is acute: reframing federal protection as 'overreach' makes the protective mechanism itself illegitimate, trapping beneficiaries without exit. Regional/state alternatives inadequate due to competitive pressure (industries relocate to low-regulation jurisdictions).
constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-CLASS EARNERS (TANGLED ROPE) — Experience genuine coordination benefit (predictable property rights, contract enforcement, lower corruption than patronage systems) alongside extraction (taxes fund redistribution they philosophically oppose, regulatory compliance costs). Can exit partially through political voice and capital flight to favorable jurisdictions, but constrained by employment, family, social ties. The doctrine provides ideological cover for limiting their tax burden while maintaining benefit from federal infrastructure. Moderate experienced extraction — significant but not total.
constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL HOLDERS & ENTREPRENEURIAL CLASS (ROPE) — Primary beneficiaries. The doctrine constrains federal taxing and regulatory authority precisely where it would limit capital mobility, cross-border trade, and wealth accumulation. Experience the constraint as pure coordination: secure property rights, predictable legal framework, minimal redistribution, ability to arbitrage between jurisdictions with different regulatory regimes. Net beneficiary status — extraction runs toward this agent. Can exit through capital flight or regulatory arbitrage if constraints are violated.
constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE & REGIONAL GOVERNMENTS (TANGLED ROPE) — Gain autonomy from federal constraint (can set labor, environmental, welfare policy independently) but face race-to-the-bottom pressure. Benefits from coordination (federal backing, capital flows to stable jurisdictions) alongside extraction (cannot enforce redistribution or environmental protection without federal cooperation, lose fiscal capacity as capital emigrates to lower-regulation zones). Suppression derives from competitive pressure between states — cannot unilaterally raise standards without capital flight. High theater (federalism rhetoric obscures actual dependency on federal infrastructure spending).
constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ORIGINALIST INTERPRETIVE COMMUNITY (PITON) — The doctrine is maintained through performative constitutional interpretation: appeals to Founding Fathers' intent (largely historically contested, unfalsifiable by modern evidence). The interpretive practice persists through institutional inertia and career incentives within law academia/courts, not because the historical evidence unambiguously supports the reading. Theater is high: detailed originalist scholarship provides cover for what is an ideological preference. Function has degraded — the doctrine no longer constrains policy in proportion to its rhetorical centrality. Courts have authorized vast federal powers (commerce clause, spending clause) while maintaining originalist rhetoric.
constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of false summit. From a universal/civilizational perspective, some degree of power limitation and individual liberty protection is inherent to sustainable governance systems: concentrating all authority in a central apparatus creates systemic fragility and predictable institutional failure. This perspective sees the doctrine as reflecting a natural law of political economy. However, the structural data contradicts this classification — identifiable beneficiaries (capital holders), identifiable victims (dependent populations), active enforcement (constitutional courts reinterpreting scope limits), and theater (originalist narratives) all indicate this is a constructed institutional arrangement, not a natural law. False summit diagnosis applies.
constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1988_reagan_limited_government_personal_liberty_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1988_reagan_limited_government_personal_liberty_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1988_reagan_limited_government_personal_liberty_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1988_reagan_limited_government_personal_liberty_doctrine, TR),
    TR >= 0.70.

:- end_tests(sotu_1988_reagan_limited_government_personal_liberty_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The doctrine extracts value from dependent populations by removing federal protections where market mechanisms cannot substitute, while concentrating gains among capital holders who benefit from reduced taxation and regulatory constraint. The value is moderate-high because genuine coordination benefits exist (predictable property rights, contract enforcement reduce transaction costs, benefit all economic classes) alongside extraction. The doctrine is not pure extraction (Snare) because it provides real coordination function for commerce and investment. Suppression (0.62): Significant suppression operates through multiple mechanisms. Structural suppression: dependent populations face material barriers (cannot relocate to alternative jurisdictions, cannot self-organize market solutions at required scale). Discursive suppression: reframing federal protection as 'overreach' and dependence as moral failure delegitimizes the protective mechanism itself, making exit (independence) ideologically mandatory while materially impossible. Competitive suppression: state governments cannot unilaterally raise environmental/labor standards without capital flight, creating downward pressure. Theater ratio (0.68): High. The doctrine maintains rhetorical centrality while actual federal scope remains vast. Reagan explicitly invoked limited government ideology while maintaining/expanding federal spending (defense, farm subsidies), enforcement of regulations (continued EPA operations), and redistributive transfers (Social Security was protected). The originalist framework (appeals to Founding Fathers' intent) provides performative cover for what is an ideological preference — the historical evidence for limiting federal scope is contested and subject to multiple coherent interpretations. Courts continue authorizing vast federal powers (commerce clause, spending clause) while maintaining originalist rhetoric. Theater increased from 1980 (0.52) to 1992 (0.75) as the gap between rhetoric and practice widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence rooted in structural position and exit options. Capital holders (institutional/arbitrage) experience the doctrine as pure coordination — secure property rights, predictable legal framework, minimal extraction via redistribution, ability to arbitrage between jurisdictions. Dependent populations (powerless/trapped) experience it as pure extraction — removal of protections where no market alternative exists, suppression via redefinition of their status as moral failure rather than structural need. Middle-class earners (moderate/constrained) experience a tangled-rope mixture — genuine coordination benefit (property rights, contract law) alongside extraction (taxes fund redistribution they oppose). State governments (organized/constrained) experience tangled rope rooted in competitive pressure — autonomy gained from federal constraint is offset by inability to enforce standards unilaterally. The originalist interpretive community (institutional/arbitrage) experiences the doctrine as fulfilling a performative role (scholarship produces career incentives; originalist framing provides legitimacy) even as courts undermine it in practice (piton). The analytical observer risks false summit reasoning — seeing the constraint as a natural law of political economy reflecting inherent limitations of centralized authority — when the structural data reveals it as a constructed institutional arrangement with identifiable beneficiaries, victims, and enforcement mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position, exit options, and beneficiary/victim status. Capital holders have low d (0.05-0.15): they benefit from the constraint (federal taxation and regulation constrained), and they have high exit options (capital mobility, arbitrage between jurisdictions). This produces negative to minimal effective extraction f(d) ≈ -0.12 to -0.01. Dependent populations have high d (0.90-0.98): they bear full cost of constraint (federal protections removed where market alternatives don't exist), and they have no exit options (trapped). This produces maximum experienced extraction f(d) ≈ 1.40. Middle-class earners have moderate d (0.50-0.65): mixed beneficiary/victim status (gain from property rights, lose from taxes funding redistribution), constrained exit options (political voice, partial capital flight). This produces moderate extraction f(d) ≈ 0.65-1.00. The tangled-rope classification requires both significant coordination function (present: property rights, contract enforcement) and asymmetric extraction (present: differential impact on dependent vs capital-holder populations), with active enforcement (present: constitutional courts, regulatory interpretation). All three requirements are met.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival pluralism. The doctrine is legitimately tangled-rope (genuine coordination function for contract/property law mixed with asymmetric extraction affecting dependent populations), not a pure coordination mechanism (rope) and not pure extraction (snare). The key to mandatrophy resolution is acknowledging that the coordination benefits are real AND distributed unequally. Everyone benefits from predictable property rights and contract enforcement (rope benefit). But capital holders benefit massively from constrained taxation/regulation while dependent populations lose federal protections (extraction rotates toward capital holders). The doctrine cannot be collapsed to a single type — it instantiates genuine mixed function with asymmetric extraction. The piton perspective reveals that the constraint is maintained partly through performative rhetoric (originalism) rather than purely through its functional necessity. The mountain perspective is a false summit diagnosis — the 'natural law' framing naturalizes what is actually a contingent political choice about federal scope, a choice that has been made differently at other historical moments (New Deal, Civil Rights era, post-WWII).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_fathers_intent_determinacy,
    'Is the Founders'' intent regarding federal scope limit determinable from historical texts, or are multiple coherent interpretations equally supported by evidence?',
    'Historiographical consensus studies; identification of contemporary Founding-era sources supporting competing scope interpretations; legal realist analysis of how different courts have derived different scope limits from the same constitutional language',
    'If intent is determinable: the doctrine is grounded in constraint language. If intent is indeterminate: the originalist framework is performative, and the constraint is maintained through rhetorical closure rather than textual reference. Reclassifies mountain perspective to false summit confirmation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_fathers_intent_determinacy, empirical, 'Whether Founding Fathers'' intent on federal scope is historically determinable').

omega_variable(
    market_mechanism_sufficiency,
    'For the populations dependent on federal protection (safety-net recipients, regulatory beneficiaries), do decentralized market mechanisms provide adequate substitutes at the scale required, or does removing federal coordination create unmet demand?',
    'Comparative analysis of outcomes pre-Reagan (higher federal scope) vs post-Reagan (lower federal scope); identification of harms that emerged when federal programs were devolved or eliminated; measurement of private sector substitution rates for public protections',
    'If market mechanisms are sufficient: snare classification may be overstated — victims have more agency through entrepreneurship/mutual aid than trapped perspective assumes. If insufficient: snare classification is confirmed — populations are locked in by structural inability of decentralized mechanisms to provide at required scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_mechanism_sufficiency, empirical, 'Whether market mechanisms provide adequate substitutes for federal protection functions').

omega_variable(
    capital_flight_constraint_interaction,
    'Does the federal scope constraint depend on capital mobility for its enforcement, or would it persist as ideology even without realistic capital exit options?',
    'Hypothetical analysis of constraint persistence in scenarios of capital immobility; measurement of policy shifts during periods of restricted capital mobility (e.g., post-2008 financial crisis, wartime economies); identification of whether rhetorical commitment to doctrine persists when exit options vanish',
    'If constraint depends on capital mobility: it is a coordination mechanism maintained by arbitrage threat (Rope or Tangled Rope for capital holders). If constraint persists without mobility: it is more purely ideological (Snare), maintained through suppression of competing narratives rather than through exit threats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_flight_constraint_interaction, conceptual, 'Whether constraint depends on capital mobility for its enforcement').

omega_variable(
    extraction_vs_legitimacy_framing,
    'Is the constraint''s suppression mechanism structural (material barriers to exiting dependent status) or discursive (redefining dependence as moral failure, making exit ideologically mandatory while materially impossible)?',
    'Discourse analysis of Reagan-era rhetoric on welfare, regulation, and dependence; measurement of stigma and shame among benefit recipients; identification of whether populations experience suppression as external barriers or internalized moral pressure',
    'If suppression is primarily discursive: the constraint is more like identity_locked (agents are cognitively captured by framing that makes exit unthinkable) than purely trapped. Reclassifies some dependent population perspectives from trapped to identity_locked, revealing cognitive capture mechanism. If structural: suppression derives from material economic barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_legitimacy_framing, empirical, 'Whether suppression is structural or discursive (identity-based)').

omega_variable(
    regulatory_regime_race_to_bottom,
    'Do state/regional governments actually engage in race-to-the-bottom competitive deregulation when federal scope is constrained, or do they maintain independent regulatory standards despite competitive pressure?',
    'Comparative analysis of state environmental and labor regulations 1980-1990 vs 2010s periods; measurement of regulatory variation across states with different competitive positions; identification of whether regulatory variance increased or decreased as federal scope was restricted',
    'If race-to-bottom is real: state governments face genuine tangled-rope condition (cannot enforce protections unilaterally). If states maintain independent standards: the suppression is overstated, and state perspectives should reclassify upward in power/agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_regime_race_to_bottom, empirical, 'Whether state governments engage in regulatory race-to-bottom under federal scope constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1988_reagan_limited_government_personal_liberty_doctrine, 1980, 1992).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reagan_theater_1980, sotu_1988_reagan_limited_government_personal_liberty_doctrine, theater_ratio, 0, 0.52).
narrative_ontology:measurement(reagan_theater_1988_sotu, sotu_1988_reagan_limited_government_personal_liberty_doctrine, theater_ratio, 4, 0.68).
narrative_ontology:measurement(reagan_theater_1992, sotu_1988_reagan_limited_government_personal_liberty_doctrine, theater_ratio, 8, 0.75).

% Extraction over time
narrative_ontology:measurement(reagan_extractiveness_1980, sotu_1988_reagan_limited_government_personal_liberty_doctrine, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(reagan_extractiveness_1988_sotu, sotu_1988_reagan_limited_government_personal_liberty_doctrine, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(reagan_extractiveness_1992, sotu_1988_reagan_limited_government_personal_liberty_doctrine, base_extractiveness, 8, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1988_reagan_limited_government_personal_liberty_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1988_reagan_limited_government_personal_liberty_doctrine, welfare_state_retrenchment).
narrative_ontology:affects_constraint(sotu_1988_reagan_limited_government_personal_liberty_doctrine, regulatory_capture_incentive_structure).
narrative_ontology:affects_constraint(sotu_1988_reagan_limited_government_personal_liberty_doctrine, federalism_competitive_dynamics).
narrative_ontology:affects_constraint(sotu_1988_reagan_limited_government_personal_liberty_doctrine, originalism_constitutional_interpretation).

% DUAL FORMULATION NOTE:
% This constraint operates as the ideological superstructure for multiple downstream institutional arrangements: welfare policy (federal scope narrowing → devolution to states → means-testing expansion), regulatory policy (federal scope narrowing → industry capture of state regulators), and constitutional interpretation (originalism as the interpretive framework legitimating scope limits). Each downstream constraint has its own ε value reflecting empirical instantiation; this story captures the overarching ideological constraint at the governance philosophy level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1988_reagan_limited_government_personal_liberty_doctrine, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
