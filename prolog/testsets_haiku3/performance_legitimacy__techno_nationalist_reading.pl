% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy: Strategic Industry Dominance
 *   domain: political_economy/state_capitalism/development_planning
 *
 * SUMMARY:
 *   A state legitimacy system grounded in technological self-sufficiency and
 *   global strategic-industry dominance directs massive capital allocations
 *   toward defense-adjacent tech sectors and designated national champions,
 *   enforced through industrial policy, state ownership, and protectionist
 *   controls. The constraint is a reading of the performance-legitimacy
 *   kernel: one way a state justifies its continued rule by pointing to what
 *   it builds and controls. This reading emphasizes security and great-power
 *   status over immediate livelihood or consumer welfare. The structural
 *   delta versus sibling readings: strategic industry dominance becomes the
 *   primary performance metric; market allocation is subordinated; capital
 *   flows to selected sectors regardless of profitability signals;
 *   beneficiaries are concentrated (defense tech, national champions);
 *   victims are dispersed (consumer sectors, rural regions, market-driven
 *   enterprises). The claim (tangled rope) reflects genuine coordination of
 *   security-critical technology development alongside asymmetric extraction
 *   from non-strategic sectors.
 *
 * KEY AGENTS:
 *   - State planning apparatus: sets priorities, allocates capital, enforces supply-chain controls — institutional power, arbitrage exit (can reallocate at will)
 *   - Defense-adjacent tech sectors: primary beneficiaries, guaranteed contracts and state funding — organized power, arbitrage exit (state dependency is exit freedom relative to market discipline)
 *   - National champions: state-backed tech firms competing globally — powerful actors, constrained exit (state stakes, government-dependent revenue)
 *   - Consumer goods sectors: capital-starved, compete against protected national champions, no state priority — moderate power, constrained exit (credit relationships, domestic market access controlled by state)
 *   - Rural livelihood producers: bear opportunity cost of capital diversion without receiving benefits — powerless, identity-locked (place-bound, kinship-dependent)
 *   - Consumer citizens: face higher consumer prices, delayed service modernization, identity-locked exit (citizenship is binding within jurisdiction)
 *   - Development economists/multilateral observers: measure outcomes and contest the founding problem's magnitude — analytical seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.78).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.68).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy: Strategic Industry Dominance").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/state_capitalism/development_planning").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '69c6f4a7-42c1-49d0-8861-7287c440ddde').
narrative_ontology:cs_kernel_codification('69c6f4a7-42c1-49d0-8861-7287c440ddde', fixed_text).
narrative_ontology:cs_authority_grounding('69c6f4a7-42c1-49d0-8861-7287c440ddde', extraction).
narrative_ontology:cs_interpretation_layer_present('69c6f4a7-42c1-49d0-8861-7287c440ddde').
narrative_ontology:cs_reading_relation('69c6f4a7-42c1-49d0-8861-7287c440ddde', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('69c6f4a7-42c1-49d0-8861-7287c440ddde', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('69c6f4a7-42c1-49d0-8861-7287c440ddde', performance_legitimacy__livelihood_security_reading, forecloses).
narrative_ontology:cs_axiom('69c6f4a7-42c1-49d0-8861-7287c440ddde', foundational, strategic_tech_dominance_as_primary_legitimacy).
narrative_ontology:cs_axiom_status(strategic_tech_dominance_as_primary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('69c6f4a7-42c1-49d0-8861-7287c440ddde', strategic_tech_dominance_as_primary_legitimacy, deontological).
narrative_ontology:cs_axiom('69c6f4a7-42c1-49d0-8861-7287c440ddde', secondary, great_power_security_justifies_consumer_sector_sacrifice).
narrative_ontology:cs_axiom_status(great_power_security_justifies_consumer_sector_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('69c6f4a7-42c1-49d0-8861-7287c440ddde', great_power_security_justifies_consumer_sector_sacrifice, instrumental).
narrative_ontology:cs_reference_frame('69c6f4a7-42c1-49d0-8861-7287c440ddde', state_directed_tech_dominance_framework).
narrative_ontology:cs_drift_state('69c6f4a7-42c1-49d0-8861-7287c440ddde', contemporary_geopolitical_tension_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('69c6f4a7-42c1-49d0-8861-7287c440ddde', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_enterprises).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_allocation_mechanism).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, rural_livelihood_producers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, consumer_citizens).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets strategic industry priorities, directs massive capital allocations to defense-adjacent tech and semiconductor manufacturing, enforces supply-chain resilience requirements, and controls export licensing for critical technologies. Justifies allocations as necessary for national security and great-power status. Administers the constraint through industrial policy, state-owned enterprises, and security vetting of foreign investment.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planning_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Receive guaranteed state contracts, preferential access to capital, subsidized inputs, tariff protection, and technology transfer mandates. Include semiconductor fabs, advanced materials, quantum computing labs, and dual-use electronics manufacturers. Their survival and expansion are decoupled from market demand; the state buys their output or subsidizes it as strategic reserve. Exit is negligible — the state is their primary customer and enforcer of market protection.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    organized, generational, arbitrage, national).

% State-selected tech firms (telecom, cloud, AI) designated for global leadership. Receive directed investment, preferential government procurement, protected domestic markets, and mandatory technology partnerships with foreign firms that seek market access. Their strategic role is to achieve global market share in areas deemed strategically sensitive. Exit options are constrained by state ownership stakes and control of their primary revenue sources.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champions, beneficiary,
    powerful, generational, constrained, global).

% Consumer electronics, appliances, furniture, textiles, and light manufacturing are starved of capital and state attention to redirect resources to strategic sectors. They face cheaper imported competition that is not protected against, rising input costs from price-inflated strategic materials, and reduced access to credit. They must absorb efficiency losses and delayed modernization while subsidizing the strategic sector's development through higher tax rates and reduced public investment in infrastructure they depend on.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors, payer,
    moderate, biographical, constrained, national).

% Private companies operating outside designated strategic sectors face capital controls, restricted access to foreign exchange, mandatory partnerships with state firms, and subordination to strategic-sector supply priorities. They compete against state-backed national champions in their own domains and cannot match subsidized pricing or guaranteed government demand. Exit options are limited: foreign investment is restricted; overseas operations face retaliation; domestic exit is slow given relationship-dependent credit access.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_enterprises, payer,
    moderate, biographical, constrained, national).

% Agricultural and small-scale manufacturing workers in regions not selected for strategic-sector development face stagnant investment, population outmigration to industrial centers, reduced access to credit and extension services, and competition from mechanization driven by strategic-sector efficiency gains. Their labor identity is tied to place; exit to the city requires breaking kinship and community bonds. They bear the opportunity cost of capital diverted to tech sectors but receive no direct benefit from strategic-sector growth.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, rural_livelihood_producers, payer,
    powerless, biographical, identity_locked, local).

% Daily-life consumers and workers experience higher prices for consumer goods (starved of investment), slower improvement in services (healthcare, education, housing lag behind strategic-sector pace), and delayed modernization of infrastructure (electricity grids prioritized for factories over household electrification). They also theoretically benefit from long-term security and great-power status, but that benefit is distant and collective while costs are immediate and individual. Exit is structurally impossible: they are citizens within the jurisdiction.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_citizens, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, consumer_citizens, beneficiary).

% International firms in semiconductors, cloud computing, and AI face export controls, market access restrictions, forced technology transfer requirements, and coordinated state purchasing that favors domestic champions. They are excluded from large markets and prevented from competing on equal terms. Their only option is accepting partial market access through joint ventures that give national champions access to their IP.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, foreign_tech_competitors, excluded,
    powerful, generational, trapped, global).

% External analysts and multilateral institutions (World Bank, IMF, academic economists) observe and measure the constraint's operation. They document capital allocation patterns, sectoral growth disparities, consumer-goods scarcity, and compare development outcomes (GDP growth, technological advancement, inequality, welfare measures) across readings of the performance-legitimacy kernel. Their analysis informs policy debate but does not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, state_planning_apparatus).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of directing scarce capital toward technologies deemed essential for security and great-power competition, rather than leaving allocation to dispersed market signals that might prioritize immediate consumer welfare. Coordinates technology acquisition, supply-chain resilience, and domestic-champion protection under unified strategic planning. Internalizes security externalities that markets would ignore.
% TRANSFER_FUNCTION: Transfers capital, preferential market access, subsidized inputs, and guaranteed government demand from consumer sectors and market-driven enterprises to defense-adjacent tech sectors and national champions. Transfers labor from regional livelihood sectors to new industrial centers. Transfers purchasing power from consumers of delayed-modernization services to strategic-sector R&D budgets. Transfers market opportunity from foreign tech firms to domestic champions via restrictions and technology-transfer mandates.
% ABSENT_VOICES: Rural populations and urban service workers who bear opportunity costs (healthcare, education, housing lag) but have no voice in strategic-sector selection. Market-driven enterprises excluded from strategic designation have no representation in capital allocation decisions. Foreign tech firms and smaller domestic competitors affected by protectionism are structurally excluded from the legitimacy conversation. Consumer-goods workers and unemployed displaced by automation in non-strategic sectors have no formal seat at strategic-priority debates.
% DISAPPEARANCE_RATIONALE: If techno-nationalist performance legitimacy and its enforcement vanished, capital would rapidly shift toward consumer sectors, services, and market-driven enterprises; consumer goods prices would fall and supply would rise; regional development would accelerate as constraints on non-strategic investment lifted. The state apparatus that currently coordinates tech dominance would either dissolve or reorganize around different legitimacy criteria (livelihood security, quantitative growth, qualitative development). Global tech competition would immediately shift as national champions lost state backing. Technology transfer mandates would cease, opening foreign investment. Export controls would relax, allowing domestic firms to serve global markets. The world does not return to pre-constraint conditions (global tech competition persists, geopolitical tensions remain), but the economic structure rearranges as capital allocation regime shifts.
% FOUNDING_PROBLEM: Strategic competition between great powers for technological dominance in defense-critical domains (semiconductors, quantum, AI, advanced materials). Concern that market-driven allocation leaves the nation technologically vulnerable in security-sensitive industries. Historical experience of technology embargoes and supply-chain weaponization by rival powers. Perceived necessity of rapid domestic capacity-building to ensure national independence from hostile technology controls.
% FOUNDING_PROBLEM_CORROBORATION: State planners and defense strategists attest the founding problem is live and acute — they cite geopolitical tensions, demonstrated technology theft, and dual-use supply vulnerabilities, with reference to specific embargo incidents and espionage cases. Development economists, multilateral institutions (World Bank, IMF), and independent technology analysts contest the magnitude and necessity of the measured response. They argue that the consumer-sector atrophy and innovation bottlenecks in non-strategic domains create offsetting economic vulnerabilities. Technology benchmarking studies confirm that strategic-sector investments have advanced capability but diverge on whether comparable outcomes would result from market-driven investment supplemented by defense procurement, which is the counterfactual no other corroborating source explicitly endorses as tested.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.61 → 0.78 over the interval) because capital flows decouple from market signals and concentrate in protected sectors; the beneficiaries gain guaranteed rents while victims subsidize them through reduced investment in their domains. Suppression is elevated (0.68) because market-driven allocation must be actively prevented — competitors excluded, foreign tech restricted, domestic capital controls enforced. Theater rises from 0.28 to 0.42 because over time the constraint requires increasingly elaborate justifications about 'strategic necessity' and 'great-power competition' to sustain support; the real function (strategic capital allocation) persists, but the performance rhetoric intensifies. Accessibility of alternatives collapses to 0.62: once the state commits to techno-nationalist dominance, exiting the national framework is structurally difficult (citizenship, capital controls, language/cultural barriers); within-nation alternatives (market-driven growth, consumer-first allocation) are technically possible but politically suppressed. Resistance is high (0.71) because consumer sectors and distributed middle classes experience tangible deprivation (consumer goods scarcity, delayed housing/healthcare modernization); they mount persistent pressure through consumer demand, entrepreneurship outside state channels, and political debate, but the state apparatus's institutional power absorbs this resistance without breaking.
 *
 * PERSPECTIVAL GAP:
 *   From the state planning apparatus seat (agenda-setter), the constraint is genuine coordination solving a real security problem; great-power competition is zero-sum and requires concentrated investment; market signals mislead by ignoring security externalities — they compute it as rope with justified asymmetry. From the consumer-goods sector seat (payer), the same structure is pure extraction — they subsidize tech sector development against their will, their exit options are constrained (capital controls, domestic market barriers), and the security benefit is distant/collective while their costs are immediate/individual — they compute it as snare or tangled rope with unjustified extraction. From the rural livelihood seat (identity-locked payer), the constraint is simultaneously invisible (they receive no direct orders) and suffocating (capital never flows to their region, youth migrate to tech centers, their labor identity becomes obsolete) — they experience it as structural economic death, not a coordination problem. Development economists observe the same material facts from an analytical seat and contest whether the founding problem justifies the measured extraction: does great-power competition really require consumer-sector starvation, or is the extraction driven by state-elite capture of legitimacy claims?
 *
 * DIRECTIONALITY LOGIC:
 *   Defense-adjacent tech and national champions are full beneficiaries: they collect guarantees, subsidies, protected markets, and technology transfer mandates without market discipline (d ≈ 0.0 to 0.15). State planning apparatus is the agenda-setter with arbitrage exit (can shift priorities at will) and institutional power (d ≈ 0.2 — benefits from administering the system but genuinely could reallocate if political pressure shifted). Consumer-goods sectors and market-driven enterprises are targets with moderate power and constrained exit: they pay through reduced capital access, higher input costs, and subordinated market position; exit is constrained by capital controls and domestic market dependencies (d ≈ 0.75 to 0.85). Rural livelihood producers are identity-locked targets with powerless position: they bear opportunity costs without direct benefit; their exit options are nearly zero (place-bound, kinship-dependent, labor skills tied to obsolescing activities); they have no seat at the table where priorities are set (d ≈ 0.95). Consumer citizens straddle: they benefit theoretically from great-power status and long-term security (d ≈ 0.4) but experience daily costs from consumer-goods delays and service modernization lags (d ≈ 0.6); the distribution across the population is asymmetric — high-income beneficiaries of national-champion employment shift toward beneficiary end, while low-income consumers shift toward target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (great-power tech competition and strategic supply-chain vulnerability) is LIVE in the authoring seat's frame — geopolitical tensions, technology embargoes, and demonstrated supply-chain weaponization are documented facts. However, the constraint is TANGLED ROPE, not ROPE, because the founding coordination problem (how to rapidly build domestic strategic-tech capacity) has become thoroughly entangled with extraction via the suppression mechanism (active prevention of market alternatives, capital controls, protectionism). The rope component is real: directing capital to semiconductors, quantum, and AI does solve genuine coordination problems that markets might underproduce. But the tangled structure is undeniable: the same rules that coordinate strategic investment also extract from non-strategic sectors and prevent them from competing — this is not a spillover cost but a designed feature (national champions compete for share of state capital against consumer sectors; protectionism is explicit policy, not accident). The classification prevents misreading this as pure rope (which would require the extraction to be incidental or efficiently minimal) or pure snare (which would require the coordination story to be cover). The mandatrophy danger is that over time, the founding problem MIGHT atrophy: if domestic strategic tech capacity is achieved and maintained, the original justification for suppressing market alternatives weakens — yet the constraint persists because state-apparatus actors benefit from administering it and national-champion firms benefit from continued protection. At that point (post-T15 in the measurement series), the constraint risks reclassifying as PITON unless political pressure forces structural adjustment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_necessity,
    'Does the measured level of great-power technological competition actually require the observed capital concentration and consumer-sector suppression, or would market-driven strategic investment plus targeted defense procurement achieve similar security outcomes at lower aggregate cost to the economy?',
    'Comparative analysis of technology outcomes in peer economies using different allocation regimes (e.g., US market-driven + defense contracts vs. state-directed tech sectors); technology-benchmarking studies controlling for R&D intensity; simulation of counterfactual market-allocation scenarios with security constraints modeled.',
    'If market-driven allocation with security overlay achieves comparable outcomes, the measured extraction (0.78) exceeds what the founding problem justifies; the constraint would reclassify as SNARE (the security story is cover) or remain TANGLED ROPE but with the extraction component exposed as surplus. If state direction is genuinely necessary, the extraction remains justified by the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_necessity, empirical, 'Whether the founding problem justifies the scale of consumer-sector sacrifice observed.').

omega_variable(
    sibling_reading_displacement,
    'When this reading''s legitimacy claims (technological dominance, great-power status) are asserted by state actors, what happens to the legitimacy claims of the sibling readings (livelihood security, qualitative development, quantitative growth)? Are they displaced, subordinated, or coexisting?',
    'Content analysis of state legitimacy narratives (official speeches, policy documents, media) coded for which readings are invoked; ethnographic study of how citizens in different sectors experience the competing legitimacy claims; political contestation analysis tracking which readings gain/lose rhetorical prominence over the interval.',
    'If techno-nationalist reading forecloses livelihood-security reading (citizens told to accept consumer-goods delays for great-power status, with no livelihood improvement), the constraints are foreclosing — one reading''s dominance suppresses the others. If readings coexist (state claims all four simultaneously, compartmentalizing different audiences), they are separate constraints with different beneficiaries. If techno-nationalist reading influences but does not foreclose others (subordinates them but does not eliminate them), the network is influences-type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_displacement, empirical, 'Whether this reading''s political dominance displaces or coexists with sibling readings'' legitimacy claims.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.68) primarily structural (capital controls, licensing barriers, market access restrictions) or partially internalized (citizens accept the need for sacrifice; workers believe tech-sector jobs are higher-status; consumer-goods workers internalize the idea that strategic sectors are ''necessary'')?',
    'Post-policy-change empirical test: if suppression mechanisms are removed (capital controls relaxed, market access opened) and suppression drops, mechanisms are structural; if suppression persists (workers continue to transfer capital, consumer-goods demand remains soft), mechanisms are internalized. Surveys of belief formation about tech-sector necessity and strategic legitimacy.',
    'If internalized, the effective suppression is higher than the 0.68 scalar suggests — workers and consumers carry the suppression independently even if external barriers weaken. This would support SNARE classification and suggest that liberalization alone would not decompress the constraint. If structural, the suppression is primarily enforcement-dependent and could weaken rapidly if enforcement erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the measured suppression is structural coercion or internalized belief.').

omega_variable(
    kernel_reading_foreclosure,
    'This reading emphasizes great-power technological dominance as the legitimate performance metric. Does this reading''s core premise — that national security via strategic-tech independence is the primary legitimacy source — logically foreclose the livelihood-security reading (which treats daily-life experience as the primary metric)? Or can a state coherently claim both?',
    'Logical coherence test: can a state simultaneously claim ''we are legitimate because we achieved tech dominance'' AND ''we are legitimate because we delivered livelihood security''? If consumer sectors are materially suppressed, both claims together are inconsistent — the choice between them is real. Ethnographic/interview study of how state actors and citizens navigate this contradiction.',
    'If the readings logically foreclose each other, then the techno-nationalist reading does FORECLOSE the livelihood-security reading, and they cannot coexist within the same legitimacy framework. This would make them competitors in a winner-take-all contest, not coexisting alternatives. If they can coexist (state achieves great-power status while improving livelihoods in other domains), they are COEXISTS_WITH. If techno-nationalist reading influences (subordinates livelihood improvement during the tech-buildup phase but promises it later), relation is INFLUENCES.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether this reading''s core premise logically forecloses the livelihood-security reading or merely displaces it politically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(perf_tr_t0, observed).
narrative_ontology:measurement(perf_tr_t3, performance_legitimacy__techno_nationalist_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement_basis(perf_tr_t3, observed).
narrative_ontology:measurement(perf_tr_t6, performance_legitimacy__techno_nationalist_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(perf_tr_t6, observed).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__techno_nationalist_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(perf_tr_t10, observed).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__techno_nationalist_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(perf_tr_t15, observed).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(perf_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement_basis(perf_be_t0, observed).
narrative_ontology:measurement(perf_be_t3, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 3, 0.66).
narrative_ontology:measurement_basis(perf_be_t3, observed).
narrative_ontology:measurement(perf_be_t6, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement_basis(perf_be_t6, observed).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement_basis(perf_be_t10, observed).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(perf_be_t15, observed).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(perf_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.56).
narrative_ontology:measurement_basis(perf_su_t0, observed).
narrative_ontology:measurement(perf_su_t3, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement_basis(perf_su_t3, observed).
narrative_ontology:measurement(perf_su_t6, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(perf_su_t6, observed).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(perf_su_t10, observed).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(perf_su_t15, observed).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(perf_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance-legitimacy kernel. The kernel is a contested commitment about how a state justifies rule through demonstrated performance. This reading instantiates it as technological self-sufficiency and great-power status. Sibling readings (quantitative_growth, qualitative_development, livelihood_security) instantiate the same kernel with different performance metrics and different beneficiary/victim structures. The ε values differ because each reading measures extraction against a different beneficiary set and reference point: techno-nationalist reading emphasizes extraction from consumer sectors to benefit defense-tech; livelihood-security reading would emphasize extraction from strategic sectors to benefit service sectors; quantitative-growth reading treats growth itself as the benefit (no zero-sum extraction, pure rope). These are structurally distinct claims, not angles on one fact. Link established via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
