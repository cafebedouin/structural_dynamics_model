% ============================================================================
% CONSTRAINT STORY: us_china_tech_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_tech_competition, []).

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
 *   constraint_id: us_china_tech_competition
 *   human_readable: US-China Technology Competition and Strategic Decoupling
 *   domain: geopolitical/technology/economics
 *
 * SUMMARY:
 *   The US-China technology competition represents a geopolitical decoupling
 *   of global technology systems along strategic power lines. Over the past
 *   decade (2016-2026), successive export controls on semiconductors, AI
 *   capabilities, design tools, and rare earth elements have fragmented
 *   supply chains that were optimized for cost and efficiency into redundant,
 *   dual-stack architectures justified by strategic autonomy and security.
 *   The constraint exhibits characteristics of all six DR types depending on
 *   the observer's structural position: the global supply chain bears maximum
 *   extraction (Snare); strategic planners on each side see coordination
 *   benefits (Rope/Tangled Rope); emerging markets face temporary exclusion
 *   (Scaffold); international standards bodies become performative (Piton).
 *   The extractiveness has increased over the interval (0.35 to 0.62) as
 *   controls have tightened and the theater ratio has climbed (0.45 to 0.68)
 *   as the rhetoric of strategic necessity has outpaced measurable threat
 *   reduction. This constraint is a diagnostic exemplar of how geopolitical
 *   narratives can naturalize extractive institutional arrangements.
 *
 * KEY AGENTS:
 *   - US Defense Industrial Base: Institutional/organized (arbitrage) — Primary beneficiary; captures technology sovereignty benefits and reduced competition
 *   - Chinese National Champions: Organized/institutional (constrained) — Protected beneficiary and constrained victim; benefits from market protection, bears cost of technology access restrictions
 *   - Global Supply Chain: Powerless (trapped) — Primary victim; bears extraction through fragmentation, redundancy, and inefficiency with no representation or exit option
 *   - Dual-Use Researchers: Powerless to moderate (trapped/constrained) — Bear extraction through publication restrictions, collaboration barriers, and geopolitical employment risk
 *   - Emerging Market Tech Ecosystems: Organized/moderate (constrained) — Secondary victim with scaffold properties; temporary exclusion but building alternative pathways
 *   - Strategic Planners: Institutional (arbitrage) — See constraint as solving coordination problem; net beneficiaries with low perceived extraction
 *   - Consumers Both Nations: Moderate (constrained) — Experience mixed extraction (higher prices, reduced choice) and coordination benefits (competition-driven innovation)
 *   - International Standards Bodies: Institutional (arbitrage declining) — Maintain formal legitimacy but functional role has partially atrophied; Piton classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_tech_competition, 0.58).
domain_priors:suppression_score(us_china_tech_competition, 0.65).
domain_priors:theater_ratio(us_china_tech_competition, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_tech_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_tech_competition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_china_tech_competition, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_tech_competition, tangled_rope).
narrative_ontology:human_readable(us_china_tech_competition, "US-China Technology Competition and Strategic Decoupling").
narrative_ontology:topic_domain(us_china_tech_competition, "geopolitical/technology/economics").

domain_priors:requires_active_enforcement(us_china_tech_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_tech_competition, defense_industrial_base).
narrative_ontology:constraint_beneficiary(us_china_tech_competition, semiconductor_champions).
narrative_ontology:constraint_beneficiary(us_china_tech_competition, national_champions_each_side).
narrative_ontology:constraint_victim(us_china_tech_competition, global_supply_chain_efficiency).
narrative_ontology:constraint_victim(us_china_tech_competition, dual_use_technology_developers).
narrative_ontology:constraint_victim(us_china_tech_competition, emerging_market_tech_workers).
narrative_ontology:constraint_victim(us_china_tech_competition, consumers_both_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SUPPLY CHAIN (SNARE) — Cannot exit the competition; bears extraction through fragmentation, redundancy, and inefficiency. Semiconductor supply chains optimized over decades are being deliberately broken by export controls and dual-stack infrastructure requirements. No agent represents the commons good; maximum experienced extraction.
constraint_indexing:constraint_classification(us_china_tech_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DUAL-USE DEVELOPERS (SNARE) — Researchers and engineers in cryptography, AI, semiconductors, and materials science face escalating restrictions on publication, collaboration, and tool access. Trapped by nationality-based export control regimes; cannot exit the competition framework. Bears extraction through loss of collaborative advantage and research velocity.
constraint_indexing:constraint_classification(us_china_tech_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: US DEFENSE INDUSTRIAL BASE (TANGLED ROPE) — Primary beneficiary with genuine coordination function: supply chain resilience, technology sovereignty, deterrence credibility. But also benefits from artificial market segmentation that reduces competition and enables pricing power. Constrained by regulatory requirements and alliance commitments; experiences extraction through supply chain stress and allied coordination demands.
constraint_indexing:constraint_classification(us_china_tech_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CHINESE CHAMPIONS (TANGLED ROPE) — Benefit from protected domestic markets, state support, and reduced foreign competition. Genuine coordination function: technology self-sufficiency, economic resilience. But constrained by component supply restrictions, design tool limitations, and export barriers. Extraction runs both directions: constraint imposed by US and allies; extraction of rents from protected market position.
constraint_indexing:constraint_classification(us_china_tech_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STRATEGIC PLANNERS (ROPE) — Both US and Chinese policy frameworks see the competition as solving a coordination problem: aligning technology investment with strategic objectives, managing technology transfer risks, ensuring domestic capability in critical sectors. Experience the constraint as coordination with arbitrage benefits (technology sovereignty, deterrence credibility). Net beneficiaries — low experienced extraction.
constraint_indexing:constraint_classification(us_china_tech_competition, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EMERGING MARKETS (SCAFFOLD) — Face temporary but severe constraints: limited access to cutting-edge components, design tools, and capital. Scaffold classification reflects the sunset clause potential: as regional tech ecosystems mature (India's semiconductor efforts, Vietnam's manufacturing, Africa's mobile-first development), the extraction through access restriction becomes less viable. Constrained but not trapped — alternative pathways are being built.
constraint_indexing:constraint_classification(us_china_tech_competition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: STANDARDS BODIES (PITON) — Institutions like IEEE, 3GPP, and ITU maintain formal function in standard-setting but are increasingly performative. Their legitimacy depends on multi-stakeholder participation, but geopolitical decoupling fragments standards development into de facto US/allied and Chinese ecosystems. The bodies persist through institutional inertia (memberships, meeting structures) while their functional coordination role has partially atrophied. Theater ratio high (0.68); effective extraction low because these bodies have limited enforcement power.
constraint_indexing:constraint_classification(us_china_tech_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: CONSUMERS (TANGLED ROPE) — Experience both coordination and extraction. Coordination benefit: competition drives R&D, innovation in chips and devices. Extraction cost: fragmented supply chains increase device prices, reduce hardware availability, force redundant technology stacks. Constrained by consumer choice limitations (cannot easily source globally optimized devices); moderate power (can switch between ecosystems within each bloc but not across).
constraint_indexing:constraint_classification(us_china_tech_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risk of naturalizing the competition as inevitable geopolitical law: 'rising powers always compete; technology competition is inherent to power transitions.' This perspective treats the constraint as emerging naturally from structural power dynamics. However, the base_properties reveal contingent institutional choices (export controls, supply chain fragmentation, standards fragmentation) rather than immutable conflict. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(us_china_tech_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_tech_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_tech_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_tech_competition, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_tech_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_tech_competition, TR),
    TR >= 0.70.

:- end_tests(us_china_tech_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value through supply chain inefficiency (estimated 15-25% markup on semiconductor costs due to dual-sourcing requirements), reduced innovation velocity in restricted domains, and opportunity costs to dual-use researchers. However, not maximal because the constraint also drives legitimate strategic capability building; some extraction is genuine defense coordination. The trajectory from 0.35 to 0.62 reflects escalation of controls and market fragmentation. Suppression (0.65): High. Multiple barriers prevent exit: nationality-based controls apply globally; researchers cannot collaborate across blocs; companies cannot source optimally; emerging markets cannot access cutting-edge tools. But suppression is not total — some technology transfer still occurs, some US-China collaboration continues in non-strategic domains, and supply chain adaptation is underway. Theater ratio (0.62): Moderate-high and rising. The rhetoric of strategic necessity is performative — much of the articulated threat (Chinese tech theft, asymmetric competition) reflects theater overlaid on structural competition. Actual threat levels are debated; perceived threat has grown faster than measurable threat. The theater supports the extraction by justifying suppression as necessary.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects power asymmetry. Institutional actors (strategic planners, defense industrial base, national champions) experience the constraint as manageable coordination with clear beneficiaries and exit options; they see Rope or Tangled Rope. Powerless actors (global supply chains, dual-use researchers, emerging market workers) experience the constraint as Snare or Scaffold — trapped or marginalized, with extraction but no representation. Moderate actors (consumers, replication groups, mid-tier tech companies) experience Tangled Rope — both coordination benefits (innovation competition) and extraction costs (fragmentation, higher prices). The analytical observer risks the Mountain perspective by naturalizing competition as geopolitical law, when the actual institutional choices (export control specifics, standards fragmentation, supply chain architecture) are contingent and revisable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each beneficiary/victim declaration maps to a structural relationship in the extraction flow. Defense industrial base is a beneficiary (arbitrage exit, institutional power, tech sovereignty benefit) — derives low d (~0.15). Chinese champions are partially beneficiary (protected market) and partially victim (component access restrictions) — mixed d (~0.35-0.45). Global supply chain is victim (trapped, powerless) — high d (~0.95). The suppression value (0.65) reflects structural barriers to exit: nationality-based controls, equipment access restrictions, research collaboration barriers. These are enforcement mechanisms maintaining the extraction. The theater value (0.62 rising to 0.68) reflects growing performative framing: 'strategic necessity' narrative justifies extraction even when threat magnitude doesn't strictly require the degree of fragmentation being imposed.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false natural law classification through omega variable specification. The 'natural law' perspective (mountain) claims geopolitical competition is immutable. But omegas reveal this is policy-contingent: alternative management trajectories exist (historical comparative analysis of past power transitions showed different decoupling patterns); irreversibility is overstated (supply chains can be rebuilt in 5-10 years if policy reverses); emerging alternatives exist (not just dependent on US-China cooperation). The Tangled Rope classification stands because genuine coordination function exists (technology sovereignty, deterrence credibility) alongside asymmetric extraction (market segmentation, suppression of dual-use research). The theater ratio rising from 0.45 to 0.68 signals Goodhart drift: as the rhetoric of strategic necessity outpaces measurable threat reduction, the constraint risks degrading from Tangled Rope toward Piton (performative maintenance). The Scaffold perspective on emerging markets provides a sunset clause: if alternatives mature within 20 years, the constraint shifts from permanent structural extraction toward temporary exclusion. This temporal structure prevents the mandatrophy by showing the constraint is revisable, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_irreversibility,
    'Are the supply chain, standards, and technology transfers that are being severed genuinely difficult to rebuild, or has the irreversibility narrative been overstated for political effect?',
    'Historical analysis of previous technology regimes after decoupling (Japan-US semiconductor cooperation after 1980s restrictions); comparison of stated decoupling costs vs actual implementation costs; forward modeling of reconstruction timelines and investment requirements',
    'If truly difficult to reverse (years/decades): tangled_rope classification stands; extraction is structural. If reversible within 5-year windows: classification shifts toward scaffold (temporary, not permanent structural extraction); perceived existential threat is theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_irreversibility, empirical, 'Reversibility and irreversibility of supply chain decoupling').

omega_variable(
    coordination_vs_conflict_driver,
    'Does the US-China competition primarily coordinate each nation''s technology investments (genuine Rope), or is the primary function to extract rents through market segmentation and reduce competitor capability (genuine Snare)?',
    'Comparative analysis of pre- and post-decoupling R&D productivity metrics; measurement of whether export controls reduce Chinese capability more than they reduce US cooperation benefits; cost-benefit analysis of domestically-sourced vs globally-optimized supply chains for strategic objectives',
    'If coordination-primary: more Rope and Tangled Rope perspectives emerge; the constraint is justified as necessary infrastructure. If extraction-primary: Snare and Piton perspectives dominate; the constraint is revealed as rent-seeking theater masquerading as strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_conflict_driver, conceptual, 'Whether competition is primarily coordination or extraction').

omega_variable(
    emerging_market_alternative_pathways,
    'Can emerging economies genuinely develop sovereign tech capability outside the US and Chinese stacks, or is the Scaffold sunset clause illusory?',
    'Tracking of India''s foundry plans, Vietnam''s semiconductor investments, Africa''s mobile development trajectories; measurement of whether alternative pathways achieve cost parity or functional equivalence within 10-20 year horizon; identification of insurmountable physics/capital barriers vs contingent policy barriers',
    'If genuine alternatives emerge: Scaffold classification confirmed; extraction is time-bound. If alternatives fail: emerging markets remain trapped; Snare perspective more accurate; theater of ''multi-polar tech development'' masks continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emerging_market_alternative_pathways, empirical, 'Feasibility of sovereign emerging-market tech development').

omega_variable(
    natural_law_vs_policy_choice,
    'Is US-China tech competition an inevitable consequence of power transitions and strategic competition (natural law), or is it a contingent institutional choice (policy-driven)?',
    'Comparative historical analysis: did previous power transitions (Britain-US, France-Germany) follow similar decoupling patterns, or were they managed differently? Counterfactual modeling: if both sides had chosen deep integration despite rising tensions, what would tech outcomes look like?',
    'If natural law: Mountain classification appropriate; constraint is immutable. If policy choice: Tangled Rope and Scaffold classifications more accurate; the constraint is maintainable and revisable; the ''inevitability'' framing is theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_policy_choice, conceptual, 'Whether competition is structural law or policy contingency').

omega_variable(
    extraction_beneficiary_stability,
    'Which agents truly benefit from the fragmented tech regime over a 20-30 year horizon? Are the claimed beneficiaries (US defense, Chinese champions) extracting value or bearing hidden costs?',
    'Long-term economic modeling: compare defense costs, R&D spending, and capability retention under fragmented vs integrated regimes; track whether protected companies actually innovate faster or slower than competitors under constraints; measure whether threat perception justifies extraction magnitude',
    'If beneficiaries are genuine and stable: extraction is real; Tangled Rope sustained. If beneficiaries erode under competition dynamics: extraction is unsustainable; constraint shifts toward Piton (inertial) classification as benefits decline while suppression persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_beneficiary_stability, empirical, 'Long-term stability of extraction benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_tech_competition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usctc_tr_t0, us_china_tech_competition, theater_ratio, 0, 0.45).
narrative_ontology:measurement(usctc_tr_t3, us_china_tech_competition, theater_ratio, 3, 0.55).
narrative_ontology:measurement(usctc_tr_t6, us_china_tech_competition, theater_ratio, 6, 0.62).
narrative_ontology:measurement(usctc_tr_t10, us_china_tech_competition, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(usctc_be_t0, us_china_tech_competition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usctc_be_t3, us_china_tech_competition, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(usctc_be_t6, us_china_tech_competition, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(usctc_be_t10, us_china_tech_competition, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_tech_competition, resource_allocation).
narrative_ontology:affects_constraint(us_china_tech_competition, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(us_china_tech_competition, ai_capability_asymmetry).
narrative_ontology:affects_constraint(us_china_tech_competition, rare_earth_supply_geopolitics).
narrative_ontology:affects_constraint(us_china_tech_competition, international_standards_fragmentation).

% DUAL FORMULATION NOTE:
% The US-China tech competition is upstream of multiple specialized constraints: semiconductor supply chain concentration (ε=0.52, Tangled Rope), AI capability asymmetry (ε=0.71, Snare), rare earth supply geopolitics (ε=0.48, Tangled Rope), and standards fragmentation (ε=0.55, Piton). Each downstream constraint has its own extractiveness reflecting domain-specific mechanisms. The competition constraint provides the geopolitical frame that enables all three; the downstream constraints show how the frame instantiates in material supply chains, technology capabilities, and institutional structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_tech_competition, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
