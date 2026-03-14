% ============================================================================
% CONSTRAINT STORY: regulatory_capture_fossil_fuels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_fossil_fuels, []).

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
 *   constraint_id: regulatory_capture_fossil_fuels
 *   human_readable: Regulatory Capture in Fossil Fuel Industries
 *   domain: energy_policy/political_economy
 *
 * SUMMARY:
 *   Regulatory capture in fossil fuel industries represents a mature,
 *   multi-institutional extraction mechanism where the primary beneficiaries
 *   (oil, gas, and coal companies) have captured the agencies nominally
 *   responsible for regulating them. The constraint exhibits genuine
 *   coordination functions — energy utilities do coordinate grid operations,
 *   pricing, and infrastructure investment — but these coordination functions
 *   are systematically biased in favor of incumbent fossil fuel technologies
 *   and against renewable alternatives. The mechanism operates through
 *   multiple channels: revolving-door employment between industry and
 *   regulatory agencies, industry funding of regulatory standard-setting,
 *   asymmetric technical expertise concentrated in fossil fuel expertise, and
 *   long-term institutional alignment where agencies develop their
 *   professional identities around energy security narratives that assume
 *   fossil fuel dominance. The constraint's extractiveness has increased over
 *   the past 20 years (from 0.48 to 0.68) as renewable alternatives have
 *   matured and the gap between regulatory preference for fossils and
 *   technical feasibility of rapid transition has widened. Theater ratio has
 *   also risen (0.42 to 0.58) because energy security justifications for
 *   fossil prioritization have become increasingly performative as renewable
 *   grid technologies have advanced — the narrative persists through
 *   institutional inertia even as its technical basis weakens.
 *
 * KEY AGENTS:
 *   - Fossil fuel extractors and energy utilities: Primary beneficiaries (institutional/arbitrage) — capture yields market access, regulatory price floors, infrastructure investment, and exclusion of competitors. Negative effective extraction.
 *   - Regulatory agencies (EPA, FERC, energy departments): Captured institutions (institutional/identity_locked) — identity fused to fossil fuel industries through decades of partnership, technical culture, and career paths. Structurally mobile but identity-locked; cannot perceive themselves as captured.
 *   - Climate system and atmospheric commons: Primary victim (powerless/trapped) — cannot exit or organize; bears full cost of delayed decarbonization through regulatory constraint on transition speed.
 *   - Renewable energy sector and workers: Secondary victim (moderate/constrained) — face regulatory delays (permitting, interconnection queuing), grid access costs, and infrastructure disadvantages. Constrained by capital requirements and incumbent network advantages.
 *   - Public health system: Victim (powerless/trapped) — bears costs of continued fossil fuel combustion (air pollution, health impacts). Structurally unable to exit or organize for regulatory protection.
 *   - Climate justice and divestment coalitions: Organized challengers (organized/constrained) — see capture as temporary; deploying legal strategies (climate tort, stranded assets), political pressure, and investor campaigns to create sunset pathway.
 *   - Energy security establishment: Institutional legitimation mechanism (institutional/arbitrage) — justifies capture through geopolitical doctrine; increasingly performative as renewable alternatives mature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_fossil_fuels, 0.68).
domain_priors:suppression_score(regulatory_capture_fossil_fuels, 0.72).
domain_priors:theater_ratio(regulatory_capture_fossil_fuels, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_fossil_fuels, extractiveness, 0.68).
narrative_ontology:constraint_metric(regulatory_capture_fossil_fuels, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(regulatory_capture_fossil_fuels, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_fossil_fuels, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_fossil_fuels, "Regulatory Capture in Fossil Fuel Industries").
narrative_ontology:topic_domain(regulatory_capture_fossil_fuels, "energy_policy/political_economy").

domain_priors:requires_active_enforcement(regulatory_capture_fossil_fuels).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_fossil_fuels, fossil_fuel_extractors).
narrative_ontology:constraint_beneficiary(regulatory_capture_fossil_fuels, petrostate_governments).
narrative_ontology:constraint_beneficiary(regulatory_capture_fossil_fuels, incumbent_energy_utilities).
narrative_ontology:constraint_victim(regulatory_capture_fossil_fuels, climate_stability_system).
narrative_ontology:constraint_victim(regulatory_capture_fossil_fuels, renewable_energy_sector).
narrative_ontology:constraint_victim(regulatory_capture_fossil_fuels, energy_transition_workers).
narrative_ontology:constraint_victim(regulatory_capture_fossil_fuels, public_health_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE SYSTEM (SNARE) — The atmosphere cannot exit or organize. Bears full extraction cost through regulatory constraint preventing rapid decarbonization. No alternatives accessible to this agent. Maximum extractiveness experienced.
constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RENEWABLE ENERGY SECTOR (TANGLED ROPE) — Experiences asymmetric extraction through regulatory delays, permitting barriers, and grid access costs imposed by incumbents. Also benefits from coordination of technical standards and grid infrastructure. Exit constrained by capital requirements and first-mover disadvantage against entrenched networks. Moderate to high extraction.
constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FOSSIL EXTRACTORS (ROPE) — Net beneficiary. Experiences the regulatory constraint primarily as a coordination mechanism: maintaining regulatory relations secures market access, price floors, and infrastructure subsidies. Can arbitrage between jurisdictions to minimize extraction. Negative effective extraction (benefit).
constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: CAPTURED REGULATORY AGENCIES (TANGLED ROPE) — Structurally mobile (could reject industry influence) but identity-fused with the industries they regulate. Agencies define their professional legitimacy through industry partnership, technical expertise developed in fossil fuel context, and revolving-door career paths. Extraction runs through these agencies even though they maintain a coordination narrative (ensuring reliable energy supply). Identity lock prevents recognition of capture.
constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: CLIMATE JUSTICE COALITION (SCAFFOLD) — Organized agents (environmental NGOs, climate litigation coalitions, youth movements) perceive the capture constraint as temporary. Legal strategies (climate tort liability, stranded asset doctrine), investor divestment campaigns, and carbon pricing advocacy aim to shift regulatory terrain. Low effective extraction because this coalition sees sunset pathway: carbon pricing regimes, fiduciary duty frameworks, and climate liability law are eroding the capture mechanism's functional basis.
constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GEOPOLITICAL ENERGY SECURITY DOCTRINE (PITON) — Fossil fuel regulation is justified through energy security, energy independence, and economic stability narratives. These justifications were functionally important when fossil fuels dominated energy infrastructure and vulnerabilities to supply disruption were severe. As renewable alternatives mature and energy security reframes around grid resilience rather than fuel independence, the doctrine persists through institutional inertia. Theater ratio high (0.58+) because the security narrative now performs legitimation more than it solves the original problem.
constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risks classifying fossil fuel regulatory capture as natural law: 'Energy-dense fuels will always be prioritized; concentrated economic interests always capture regulators; institutions naturally preserve incumbent positions.' This perspective naturalizes what are contingent political arrangements. The structural data shows this is Tangled Rope with high suppression, not Mountain — the capture is enforced and defended, not immutable.
constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_fossil_fuels_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_fossil_fuels, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_fossil_fuels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_fossil_fuels, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_fossil_fuels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The fossil fuel industry captures regulatory preference in ways that create measurable extraction from renewable competitors and climate mitigation efforts. The extraction includes: regulatory delays to renewable projects (average 3-5 year permitting vs. months for fossil plants), interconnection queue discrimination, subsidies and tax preferences for fossil fuels ($5-7 trillion globally per IMF accounting), and regulatory exclusion of climate costs from energy pricing. However, extractiveness is not maximal (0.95+) because the constraint still permits some renewable deployment, some jurisdictions are moving toward climate-aligned policy, and incumbent utilities are slowly building renewable capacity under regulatory pressure. The moderate-to-high value reflects that extraction is real and systematic but not absolute. Suppression (0.72): Very high. Significant structural barriers prevent actors from exiting the capture constraint: renewable developers cannot avoid regulatory jurisdiction, countries cannot opt out of global energy markets, workers cannot costlessly shift to alternative sectors, and the climate system cannot protect itself through exit. Suppression mechanisms include legal authority (regulatory agencies have formal power), capital concentration (fossil fuel industries control energy infrastructure globally), political access asymmetry (fossil fuel lobbying budgets dwarf environmental group spending), and institutional inertia. Theater ratio (0.58): Moderate-high. Energy security doctrine and technical safety narratives (reliability, grid stability, dispatchability) perform significant legitimation of fossil fuel regulatory preference, even as renewable technologies have matured to the point where these justifications are increasingly performative. The theater has grown over 30 years because the technical basis for fossil prioritization has weakened (battery storage cost has fallen 89% since 2010; wind and solar are now cheapest sources of new capacity in most jurisdictions) while the regulatory narrative persists. The narrative doesn't reflect current technical reality; it persists through institutional habit and institutional interest.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is maximal. Fossil extractors in the 'arbitrage' exit category perceive the constraint as a Rope coordination mechanism that enables reliable energy supply — they experience negative effective extraction (subsidy). Renewable developers in the 'constrained' exit category perceive the same regulatory regime as systematic discrimination — they experience high extraction (0.70+). Captured regulatory agencies in the 'identity_locked' exit category perceive themselves as neutral technical experts coordinating energy policy; they do not perceive the industry influence as extraction because their identity has fused with the industry's interests. The climate system perceives pure extraction (snare) with no mitigation pathway available through regulatory exit. The climate justice coalition perceives a temporary constraint (scaffold) with a sunset pathway through legal and political shifts. These gaps are not minor differences in emphasis; they reflect fundamentally incommensurate structural positions. The same regulatory rule (e.g., a long interconnection queue for renewables) appears as beneficial coordination to the incumbent (protects grid stability, prevents destabilization) and as predatory extraction to the renewable developer (delays capital returns, raises effective cost of capital). The gap cannot be resolved by better information or dialogue; it flows from structural asymmetry in how exit options and power aggregate the extractiveness experienced by each agent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (fossil extractors with arbitrage exit): d ≈ 0.05. The sigmoid f(d) produces ~-0.12, yielding negative effective extractiveness — these agents perceive the constraint as a subsidy. Captured agency directionality (institutions with identity_locked exit): d ≈ 0.25. The sigmoid produces ~0.02, yielding near-zero effective extractiveness from the agency's perspective — they perceive the constraint as neutral technical administration, with institutional identity aligned to incumbent interests treated as normal professionalism. Renewable sector directionality (moderate power with constrained exit): d ≈ 0.70. The sigmoid produces ~1.10, multiplying base extractiveness (0.68) to effective chi ≈ 0.75 — they experience high extraction. Powerless victims (climate, public health with trapped exit): d ≈ 0.95. The sigmoid produces ~1.42, yielding chi ≈ 0.96 — maximum experienced extraction. Organized challengers (climate coalition with constrained exit): d ≈ 0.55. The sigmoid produces ~0.75, yielding chi ≈ 0.51 — moderate extraction, but with agency to reduce it through collective action. The directionality computations show why no single perspective is 'correct': the constraint structure produces genuinely different extraction vectors for different agents. The beneficiary experiences subsidy; the victim experiences predation; the captured agency experiences neutral administration. These are not perceptual errors; they are structural facts about how the constraint redistributes resources and power.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through multi-perspectival classification that shows all types except Mountain are legitimate. From the beneficiary perspective: Rope (pure coordination with negative extraction). From the captured agency perspective: Rope with identity_lock (perceived coordination, actual asymmetric extraction hidden by identity fusion). From the renewable sector perspective: Tangled Rope (mixed coordination for grid operations + asymmetric extraction against renewables). From the climate/powerless perspective: Snare (pure extraction, no coordination function for this agent, no exit). From the organized coalition perspective: Scaffold with sunset (temporary constraint with credible exit pathway via legal/political shift). From the geopolitical doctrine perspective: Mountain / false summit (naturalizes contingent institutional choice as immutable energy security necessity). The mandatrophy is not 'which is the real type?' but 'what is the presheaf of types across observation positions?' The analytical insight is that the same constraint produces fundamentally different experienced types depending on structural position. Mandatrophy is resolved by rejecting the assumption that constraints have a single type; they have a sheaf of types over the space of observer positions. The false summit classification (Mountain from analytical context) is the diagnostic signal: claiming that regulatory capture is natural law is an abuse of the mountain category; it naturalizes a political choice. This reveals why mandatrophy matters — it prevents weaponization of 'natural law' framing to justify contingent institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_agency_vs_structure,
    'Is regulatory capture fundamentally a structural feature of energy policy (institutional incentives force agencies to coordinate with incumbents) or primarily achieved through deliberate industry agency (lobbying, funding, revolving doors)?',
    'Counterfactual institutional design: would agencies with structural independence (long fixed terms, performance metrics divorced from incumbent preferences, insulated procurement) show significantly reduced capture? Comparative analysis of regulatory agencies across jurisdictions with different institutional structures.',
    'If structural: capture is more resilient to personnel or ethics changes; institutional redesign is necessary. If agentic: stronger governance, transparency, and accountability mechanisms could reduce capture without structural reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_agency_vs_structure, empirical, 'Whether capture is structural institutional feature or agentic industry influence').

omega_variable(
    renewable_transition_ceiling,
    'What is the technical and economic ceiling for renewable energy penetration without wholesale grid architecture redesign? Does this ceiling create a permanent structural coordination role for fossil fuels?',
    'Grid modeling at 80%+ renewable penetration; historical analysis of grid modernization timelines; cost-benefit analysis of storage and demand flexibility vs. renewable curtailment.',
    'If ceiling is high (>95% feasible within 15 years): fossil fuel regulatory capture is purely extractive rent-seeking, not coordination for system stability. If ceiling is lower (<70% without redesign): capture partly reflects genuine coordination need for dispatchable capacity; classification shifts toward lower-extraction Rope perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_transition_ceiling, empirical, 'Technical feasibility of high renewable penetration without fossil fuel backbone').

omega_variable(
    identity_lock_reversibility_in_agencies,
    'Can regulatory agencies that are identity-locked to fossil fuel industries (career structures, technical cultures, institutional partnerships built over decades) shift identity allegiance to renewable/climate objectives, or does identity lock prove sticky even under strong institutional pressure?',
    'Longitudinal study of agencies transitioning to climate/renewable mandates (e.g., rebranding of energy departments, integration of climate offices); turnover rates of leadership and technical staff; analysis of regulatory outputs before/after mandate shifts; interview data on whether agency staff perceive identity shift or subordination.',
    'If reversible: identity-locked institutional perspective can shift; agencies can exit capture through conscious identity reconstruction. If sticky: capture persists through identity adhesion even with policy direction change; requires generational turnover or structural dissolution/replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility_in_agencies, empirical, 'Whether identity lock in regulatory agencies is reversible under institutional pressure').

omega_variable(
    suppression_mechanism_legality_vs_informal,
    'Is the measured suppression (0.72) primarily enforced through legal and regulatory barriers (explicit rules, licensing, grid access restrictions) or through informal structural barriers (first-mover advantages, capital concentration, political access asymmetries)?',
    'Decomposition of explicit regulatory barriers vs. informal economic/political barriers; analysis of renewable energy project delays attributed to specific regulatory requirements vs. capital/market factors; jurisdictional comparison of renewables adoption rates when legal barriers are removed but capital/incumbent structures persist.',
    'If legal suppression is dominant: rapid policy reform could unlock transition. If informal suppression is dominant: legal reform alone is insufficient; requires capital restructuring and incumbent displacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_legality_vs_informal, empirical, 'Proportion of suppression that is legal/regulatory vs. informal structural').

omega_variable(
    climate_litigation_sunset_credibility,
    'Do climate tort liability frameworks and stranded asset doctrines (the mechanism proposed by the scaffold perspective for sunset) represent a credible long-term pressure that will undermine capture, or are they vulnerable to legal/political countermeasures that perpetuate capture?',
    'Tracking of climate litigation outcomes, especially cases seeking material damages from fossil fuel companies; analysis of stranded asset writedowns by major energy companies; political economy study of legal/legislative responses to climate liability frameworks; comparison with historical liability regimes that succeeded vs. failed to shift industry behavior.',
    'If credible: scaffold sunset timeline is realistic; capture constraint is genuinely temporary. If vulnerable to countermeasures: scaffold is aspirational; capture persists and may entrench further as incumbents face existential threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_litigation_sunset_credibility, empirical, 'Whether climate liability frameworks can sustainably shift regulatory terrain').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_fossil_fuels, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_fossil_fuels, theater_ratio, 0, 0.42).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capture_fossil_fuels, theater_ratio, 10, 0.5).
narrative_ontology:measurement(regcap_tr_t20, regulatory_capture_fossil_fuels, theater_ratio, 20, 0.58).
narrative_ontology:measurement(regcap_tr_t30, regulatory_capture_fossil_fuels, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_fossil_fuels, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(regcap_be_t10, regulatory_capture_fossil_fuels, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(regcap_be_t20, regulatory_capture_fossil_fuels, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(regcap_be_t30, regulatory_capture_fossil_fuels, base_extractiveness, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_fossil_fuels, resource_allocation).
narrative_ontology:affects_constraint(regulatory_capture_fossil_fuels, renewable_energy_deployment_barriers).
narrative_ontology:affects_constraint(regulatory_capture_fossil_fuels, carbon_pricing_regulatory_resistance).
narrative_ontology:affects_constraint(regulatory_capture_fossil_fuels, fossil_fuel_subsidies_lock_in).

% DUAL FORMULATION NOTE:
% Regulatory capture in fossil fuels is upstream of multiple downstream energy policy constraints (renewable barriers, carbon pricing resistance, subsidy lock-in). The capture mechanism operates at the institutional level (regulatory agency capture) but produces measurable effects on downstream technical and economic constraints. The family structure reflects that capture is a meta-constraint that modulates the extractiveness of multiple energy policy mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_fossil_fuels, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
