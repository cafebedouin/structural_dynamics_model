% ============================================================================
% CONSTRAINT STORY: rare_earth_seabed_mining
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_seabed_mining, []).

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
 *   constraint_id: rare_earth_seabed_mining
 *   human_readable: Deep-Sea Rare Earth Mining
 *   domain: economic/technological/environmental
 *
 * SUMMARY:
 *   Deep-sea rare earth mining represents a structural collision between
 *   global materials scarcity (driving demand for new sources) and deep-sea
 *   ecosystem vulnerability (creating concentrated harm to powerless agents).
 *   Japan's test mining near Minamitorishima Island is a proof-of-concept for
 *   extracting rare-earth-bearing polymetallic nodules and sediments from
 *   abyssal zones. The constraint exhibits the tangled-rope signature:
 *   genuine coordination function (solving rare earth supply bottleneck for
 *   green technology transition globally) hybridized with asymmetric
 *   extraction (externalizing ecosystem damage and distributing benefits
 *   narrowly to wealthy manufacturing economies). The theater ratio (0.48)
 *   reflects moderate performative content: the operation is framed as
 *   technologically inevitable and necessary for climate transition,
 *   naturalizing a choice that is actually contingent on comparing cost
 *   structures and distribution across actors. Extractiveness has risen from
 *   0.35 (initial exploratory stage) to 0.58 (scaling toward commercial
 *   viability) as the constraint moves from potential to actual extraction.
 *   The suppression value (0.65) reflects significant coercive barriers:
 *   deep-sea ecosystems and coastal fishing communities have no voice in
 *   decisions made by distant governments and corporations; international
 *   maritime governance is fragmented; powerless agents face information
 *   asymmetry and cannot exit.
 *
 * KEY AGENTS:
 *   - Deep-Sea Ecosystems: Primary victim (powerless/trapped) — bears full cost of habitat destruction, sediment plumes, species loss
 *   - Coastal Fishing Communities: Primary victim (powerless/trapped) — lose access to fish stocks; trapped by geography and capital investment
 *   - Developing Nations (Rare Earth Suppliers): Secondary victim (moderate/constrained) — face competitive price pressure and supply diversification; constrained exit through economic dependency
 *   - Japan Mining Operations: Primary beneficiary (institutional/arbitrage) — gains secure supply access, strategic positioning in materials markets
 *   - Technology Manufacturers & Energy Providers: Primary beneficiary (institutional/arbitrage) — lower rare earth costs for green technology production
 *   - International Environmental Governance: Hybrid actor (organized/constrained) — provides coordination function but faces enforcement constraints from sovereignty fragmentation
 *   - Alternative Supply Coalition: Hybrid actor (organized/constrained) — developing circular economy and substitution pathways; suppressed by short-term cost incentives favoring new mining
 *   - Analytical Observer: Civilizational view (analytical/analytical) — assesses whether genuine scarcity justifies extraction or whether distribution choices and market concentration artificially inflate apparent necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_seabed_mining, 0.58).
domain_priors:suppression_score(rare_earth_seabed_mining, 0.65).
domain_priors:theater_ratio(rare_earth_seabed_mining, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_seabed_mining, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_seabed_mining, tangled_rope).
narrative_ontology:human_readable(rare_earth_seabed_mining, "Deep-Sea Rare Earth Mining").
narrative_ontology:topic_domain(rare_earth_seabed_mining, "economic/technological/environmental").

domain_priors:requires_active_enforcement(rare_earth_seabed_mining).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, japan_mining_operations).
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, technology_manufacturers).
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, energy_infrastructure_providers).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, deep_sea_ecosystems).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, coastal_fishing_communities).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, global_climate_regulation).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, developing_nations_rare_earth_suppliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEEP-SEA ECOSYSTEM (SNARE) — Cannot organize, cannot exit, cannot negotiate. Bears full extraction cost through habitat destruction, species loss, sediment plume contamination. Zero degrees of freedom. Powerless agent in a trapped context over generational timescales — the ecosystem experiences this as pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COASTAL FISHING COMMUNITIES (SNARE) — Localized victims with trapped exit. Cannot shift fishing grounds without catastrophic economic loss. Trapped by geography, capital investment in boats and permits, and dependence on fish stocks. Experience extraction as loss of resource access with no meaningful coordination benefit. Suppression is high — cultural and economic barriers prevent organization or political voice in deep-sea mining decisions made by distant governments and corporations.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEVELOPING NATIONS / RARE EARTH SUPPLIERS (TANGLED ROPE) — Face coordinated extraction through mineral-dependent economies but also stand to benefit from diversified supply sources. Exit is constrained but possible (economic diversification, alternative employment). The constraint hybridizes: deep-sea mining threatens their monopoly pricing power for terrestrial rare earths (coordination benefit for supply security globally, extraction cost for them specifically). Suppression is high due to technological barriers and capital requirements, but technological sovereignty is theoretically possible.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: JAPAN MINING OPERATIONS / TECHNOLOGY MANUFACTURERS (ROPE) — Primary beneficiaries. Experience the constraint as a pure coordination mechanism: securing rare earth supply solves the supply-chain bottleneck for green technology and electronics manufacturing. Exit is available (arbitrage) — if deep-sea mining becomes unviable, alternative supply routes exist. The extraction runs toward these actors (cost to environment, to competitors, to developing-nation suppliers becomes gain for them). Suppression is moderate — the technical barriers they face are high, but they have capital and institutional capacity to overcome them. They benefit from the coordination function (solving global rare earth scarcity) while externalizing costs to powerless agents.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL ENVIRONMENTAL GOVERNANCE (TANGLED ROPE) — Organized actors (UN bodies, maritime treaties, environmental NGOs) see coordination value (managing competing claims to deep-sea resources, establishing environmental standards) but face enforced constraints: national sovereignty over exclusive economic zones creates fragmented authority. Exit is theoretically available (stronger binding treaties) but politically constrained by veto power of mining-state actors. This is a classic hybrid: genuine coordination function (preventing destructive race-to-the-bottom) but asymmetric extraction where wealthier states sidestep constraints through selective participation or legal ambiguity.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE SUPPLY TECHNOLOGY COALITION (SCAFFOLD) — Organized actors (battery recycling programs, rare-earth-free motor designers, alternative material researchers) see deep-sea mining as a temporary problem with a structural sunset: rare earth demand can be reduced through circular economy, substitution, and efficiency. The constraint operates under suppression but with a genuine exit mechanism (technology transition over 15-30 years). Theater is low — this perspective is driven by functional coordination around alternative pathways, not performative compliance. Sunset is real if investment accelerates in recycling and substitution.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLONIAL EXTRACTION LEGACY INSTITUTIONS (PITON) — Historical institutional pattern (resource extraction from periphery to core, externalized environmental costs, powerless populations absorbing damage) persists into deep-sea domain. Functionally degraded (supply chain resilience arguments are weak — land-based mining in friendly nations is still viable; rare earth scarcity is partly artificial through market consolidation) but maintained through institutional inertia and narrative framing ('technological necessity'). Theater ratio is moderate-to-high — the mining operation is presented as inevitable technical progress, naturalizing what is actually a choice about who bears costs.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, deep-sea rare earth mining hybridizes genuine coordination (solving materials scarcity for zero-carbon energy transition) with extractive asymmetry (externalizing deep-sea ecosystem costs to powerless agents, concentrating benefits in wealthy manufacturing economies). The constraint is not inevitable: alternative supply chains (recycling, substitution, terrestrial mining in regulated contexts) are feasible at higher cost and longer timelines. Theater ratio reflects the performance of technological inevitability around a choice that is genuinely contingent. Suppression is structural — powerless agents (fishing communities, marine ecosystems) have no voice in decisions, while organized actors (governments, corporations) have veto power.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_seabed_mining_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_seabed_mining, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_seabed_mining, TR),
    TR >= 0.70.

:- end_tests(rare_earth_seabed_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Japan and manufacturing economies capture supply-security and cost benefits that would flow to consumers globally, but much of this is legitimate gain from solving a real coordination problem (rare earth supply bottleneck). The extractiveness is not as severe as a pure rent-extraction snare because the coordination function is genuine — rare earths do have real scarcity, and supply diversification does enable green energy transition. However, the distribution is asymmetric: benefits accrue to wealthy manufacturing economies; costs accrue to powerless agents (ecosystems, fishing communities) with no say in the decision. Extractiveness would be 0.35 if alternatives were unavailable; it rises to 0.58 as deep-sea mining becomes viable because decision-makers now face a choice they are exercising in ways that concentrate benefits and externalize costs. Suppression (0.65): High. Deep-sea ecosystems cannot organize or exit. Coastal fishing communities face information asymmetry, capital barriers, and political marginalization. Developing nations have constrained exit (economic dependency on mineral exports). International governance is fragmented — no binding enforcement mechanism exists for deep-sea environmental protection. National governments can unilaterally pursue seabed mining within their EEZs despite spillover effects. Theater ratio (0.48): Moderate. The narrative framing emphasizes technological necessity and climate urgency (performative content), but the operation has genuine functional content (materials production). Performativity increases as distance grows between decision-makers and cost-bearers, and as alternative supply routes (substitution, recycling, terrestrial mining) become available but politically underinvested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The primary beneficiary (Japan/manufacturers) sees coordination (Rope) — solving a real materials bottleneck. The primary victim (deep-sea ecosystem) sees pure extraction (Snare) — no coordination benefit, no exit, total cost absorption. Coastal fishing communities also see Snare — extraction of their livelihood. Developing-nation suppliers see Tangled Rope — threatened by supply diversification but also potentially freed from dependency if technology diffuses. International governance sees Tangled Rope — coordination function (preventing worse outcomes) but enforcement constraints (extraction asymmetry). The alternative supply coalition sees Scaffold — deep-sea mining is temporary, technology substitution provides a sunset. The legacy colonial-extraction institution sees Piton — the modern form of resource extraction from periphery to core, maintained through inertia and ideological framing rather than actual economic necessity. No agent sees the same constraint as any other agent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries experience low directionality (d ≈ 0.15-0.25) because they are primary extractors with arbitrage options: if deep-sea mining becomes infeasible, alternative supply chains exist (at higher cost, longer timelines, but available). Victims experience high directionality (d ≈ 0.85-0.95) because they are trapped: ecosystems have no exit; fishing communities cannot relocate; developing-nation suppliers face constrained exit through dependency. The sigmoid f(d) amplifies experienced extraction for trapped victims. Organized international governance experiences moderate directionality (d ≈ 0.50-0.60) because their exit is constrained by national sovereignty but theoretically available through stronger binding treaties — they experience both costs (enforcement failure) and potential benefits (coordinating resource governance).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through recognition that the 'inevitable necessity' framing naturalizes a choice. Deep-sea mining is tangled rope, not mountain. The genuine coordination function (rare earth supply) does not require seabed mining — it requires a choice to pursue seabed mining rather than invest equally in terrestrial mining reform, circular economy, and substitution technology. The theater ratio indicates performativity in this choice: climate urgency is invoked to bypass environmental governance and distribute costs to powerless agents. The constraint is extractive because decision-makers face alternatives (more expensive, longer timelines, but available) and choose the pathway that concentrates benefits among themselves and externalizes costs. If rare earth scarcity were truly immutable and seabed mining the only path, the constraint would approach Mountain classification (ε → 0.25, no alternatives). The fact that alternatives exist but are politically disfavored reveals the constraint as a social choice encoded in institutions, not a law of nature. The tangled-rope classification resolves the mandatrophy: genuine coordination value justifies some extraction, but asymmetric distribution (powerless agents bear full cost) and suppression (they cannot refuse) move it past pure rope into hybrid territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecosystem_recovery_timeline,
    'Can deep-sea ecosystems recover from rare earth mining sediment plumes and habitat destruction within a century?',
    'Long-term ecological monitoring of test sites; modeling of sediment plume dispersal and benthic community recovery rates; comparison with similar industrial deep-sea disturbances (trawling, cable laying)',
    'If recovery is possible within 50 years: extraction is bounded, constraint may degrade to Scaffold. If recovery is decadal or non-existent: Snare classification is confirmed, irreversible harm makes exit impossible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecosystem_recovery_timeline, empirical, 'Whether deep-sea ecosystems can recover from mining impacts').

omega_variable(
    terrestrial_mining_cost_comparison,
    'Is deep-sea mining actually cheaper than intensified terrestrial rare earth mining in regulated jurisdictions, accounting for exploration, environmental remediation, and technological risk?',
    'Full-lifecycle cost accounting: seabed vs terrestrial operations, including equipment failure rates, supply chain complexity, regulatory compliance, environmental insurance. Comparison with hypothetical scenario of 50% increase in terrestrial mining capacity.',
    'If seabed is genuinely cheaper: extraction is economically rational but distributes costs regressively (powerless agents bear ecological damage so wealthy economies get lower material costs). If seabed is economically marginal: constraint is largely driven by strategic positioning and supply-chain risk perception rather than true scarcity — reveals extractive framing of inevitable necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terrestrial_mining_cost_comparison, empirical, 'Full-cost comparison of seabed vs terrestrial rare earth mining').

omega_variable(
    circular_economy_substitution_ceiling,
    'What is the technical ceiling for meeting rare earth demand through recycling and substitution over the next 30 years? Is 50% demand reduction feasible?',
    'Materials flow analysis; technical feasibility studies for rare-earth-free motor designs and battery chemistries; pilot programs for rare earth recovery from e-waste and industrial scrap',
    'If 50%+ demand reduction is feasible by 2050: Scaffold perspective is structural — deep-sea mining is temporary. If ceiling is <20%: continued deep-sea mining becomes endemic, snare extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circular_economy_substitution_ceiling, empirical, 'Maximum feasible rare earth demand reduction through substitution and recycling').

omega_variable(
    indigenous_maritime_rights_enforcement,
    'Can Pacific island nations and indigenous maritime communities enforce exclusion of mining from their exclusive economic zones, or is enforcement capacity too weak?',
    'Analysis of legal standing, enforcement mechanisms, and enforcement costs; case studies of successful vs failed resource protection in EEZs; assessment of coalition power among island nations',
    'If enforcement is viable: constraint can be localized, reducing victim population to those in participating zones. If enforcement is weak: powerless agents remain trapped globally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_maritime_rights_enforcement, empirical, 'Whether Pacific island nations can enforce mining exclusion in their EEZs').

omega_variable(
    technological_sovereignty_diffusion,
    'Will rare earth mining and processing technology transfer to developing nations, or remain concentrated in wealthy mining operators?',
    'Patent analysis; technology transfer agreements; capacity-building outcomes in nations with terrestrial rare earth deposits; successful vs failed tech transfer case studies',
    'If technology diffuses: developing-nation victims become moderate agents with constrained exit and potential future arbitrage. If concentrated: structural asymmetry persists, tangled rope ossifies into snare for developing nations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_sovereignty_diffusion, conceptual, 'Whether rare earth mining technology will diffuse to developing nations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_seabed_mining, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resm_tr_t0, rare_earth_seabed_mining, theater_ratio, 0, 0.42).
narrative_ontology:measurement(resm_tr_t10, rare_earth_seabed_mining, theater_ratio, 10, 0.45).
narrative_ontology:measurement(resm_tr_t20, rare_earth_seabed_mining, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(resm_be_t0, rare_earth_seabed_mining, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(resm_be_t10, rare_earth_seabed_mining, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(resm_be_t20, rare_earth_seabed_mining, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_seabed_mining, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_seabed_mining, rare_earth_supply_concentration).
narrative_ontology:affects_constraint(rare_earth_seabed_mining, ocean_governance_fragmentation).
narrative_ontology:affects_constraint(rare_earth_seabed_mining, green_energy_material_bottleneck).

% DUAL FORMULATION NOTE:
% Deep-sea rare earth mining is downstream of supply-chain risk perception and upstream of material-cost implications for green energy deployment. The constraint family includes: (1) rare_earth_supply_concentration (ε ≈ 0.42, Tangled Rope) — terrestrial mining monopoly and geopolitical dependency, (2) ocean_governance_fragmentation (ε ≈ 0.55, Tangled Rope) — lack of binding international enforcement creates races-to-the-bottom in deep-sea resource extraction, (3) green_energy_material_bottleneck (ε ≈ 0.35, Rope) — genuine coordination problem of scaling clean technology. Deep-sea mining affects all three: it relieves #3 (bottleneck) by creating alternative supply, intensifies #2 (governance fragmentation) through competing maritime claims, and threatens to break #1 (supply monopoly) if successful.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_seabed_mining, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
