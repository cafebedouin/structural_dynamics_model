% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Performance Legitimacy — Techno-Nationalist Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint story captures the techno-nationalist reading of
 *   performance legitimacy: the state's right to rule is grounded in
 *   achieving technological self-sufficiency and global leadership in
 *   strategic industries (semiconductors, AI, quantum, aerospace, biotech) to
 *   ensure national security and great-power status. The arrangement directs
 *   massive capital, talent, and policy support to designated national
 *   champions and defense-adjacent sectors regardless of market signals,
 *   while suppressing consumer-oriented sectors and market-driven allocation
 *   through capital controls, credit guidance, export restrictions, and
 *   procurement mandates. The coordination function is genuine —
 *   late-development catch-up in strategic technologies requires solving
 *   collective action problems in R&D, standards, and supply chains that
 *   markets alone undersupply. But the extraction is substantial and growing:
 *   national champions capture rents via protected markets and directed
 *   credit, while consumer welfare and private SME dynamism bear the
 *   opportunity cost. The constraint requires active enforcement (investment
 *   guidance, export controls, entity lists, data localization) and has no
 *   sunset clause — the 'self-sufficiency' horizon recedes as the frontier
 *   advances.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.78).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.72).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Performance Legitimacy — Techno-Nationalist Reading").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, 'a5ce74e7-b2e9-4548-94ba-762c17834716').
narrative_ontology:cs_kernel_codification('a5ce74e7-b2e9-4548-94ba-762c17834716', distributed).
narrative_ontology:cs_authority_grounding('a5ce74e7-b2e9-4548-94ba-762c17834716', extraction).
narrative_ontology:cs_interpretation_layer_present('a5ce74e7-b2e9-4548-94ba-762c17834716').
narrative_ontology:cs_reading_relation('a5ce74e7-b2e9-4548-94ba-762c17834716', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('a5ce74e7-b2e9-4548-94ba-762c17834716', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5ce74e7-b2e9-4548-94ba-762c17834716', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('a5ce74e7-b2e9-4548-94ba-762c17834716', foundational, technological_sovereignty_is_existential).
narrative_ontology:cs_axiom_status(technological_sovereignty_is_existential, holdable).
narrative_ontology:cs_axiom_grounding('a5ce74e7-b2e9-4548-94ba-762c17834716', technological_sovereignty_is_existential, deontological).
narrative_ontology:cs_axiom('a5ce74e7-b2e9-4548-94ba-762c17834716', foundational, state_directed_allocation_necessary_for_strategic_sectors).
narrative_ontology:cs_axiom_status(state_directed_allocation_necessary_for_strategic_sectors, holdable).
narrative_ontology:cs_axiom_grounding('a5ce74e7-b2e9-4548-94ba-762c17834716', state_directed_allocation_necessary_for_strategic_sectors, instrumental).
narrative_ontology:cs_axiom('a5ce74e7-b2e9-4548-94ba-762c17834716', secondary, great_power_status_requires_strategic_autonomy).
narrative_ontology:cs_axiom_status(great_power_status_requires_strategic_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a5ce74e7-b2e9-4548-94ba-762c17834716', great_power_status_requires_strategic_autonomy, conventional).
narrative_ontology:cs_reference_frame('a5ce74e7-b2e9-4548-94ba-762c17834716', post_reform_opening_vulnerability).
narrative_ontology:cs_drift_state('a5ce74e7-b2e9-4548-94ba-762c17834716', contemporary_great_power_competition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a5ce74e7-b2e9-4548-94ba-762c17834716', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, strategic_state_owned_enterprises).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planning_agencies).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, private_sme_tech_firms).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, household_consumption).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, strategic_state_owned_enterprises).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, technological_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, great_power_status_requires_strategic_autonomy).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, state_directed_innovation_superiority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the strategic industry targeting framework: approve investment projects, allocate directed credit, manage entity lists, negotiate technology transfer. They control the constraint's parameters and capture bureaucratic authority and budget from its operation. Exit means leaving the planning apparatus for academia or SOE leadership — arbitrage-grade.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planning_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive preferential credit, protected domestic markets, state procurement contracts, and regulatory moats. They are the primary extraction capturers. Exit is constrained: they depend on the state relationship for their position, but some have global operations that provide partial diversification.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champion_firms, beneficiary,
    powerful, biographical, constrained, global).

% Military-civil fusion enterprises, research institutes, and dual-use technology firms. They receive the highest-priority funding and talent allocation. Their identity is fused to the national security mission — exit is not just economically costly but professionally and ideologically unthinkable (identity_locked).
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_tech_sectors, beneficiary,
    organized, generational, identity_locked, national).

% SOEs in semiconductors, aerospace, shipbuilding, nuclear. They capture directed investment and monopoly rents but also bear the cost of fulfilling state-mandated missions that may be commercially unviable. Dual-positioned: beneficiaries of the constraint's resource flow, payers of its quasi-fiscal burdens.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, strategic_state_owned_enterprises, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, strategic_state_owned_enterprises, payer).

% Light manufacturing, services, retail — sectors starved of credit and talent as capital is redirected to strategic industries. They face regulatory burdens (data localization, cybersecurity reviews) designed for strategic sectors. Exit is constrained: domestic market access requires compliance, but some firms relocate production or list offshore.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors, payer,
    moderate, biographical, constrained, national).

% The price mechanism, private venture capital, and market entry/exit as allocative forces. Suppressed by credit guidance, industrial policies, administrative approvals, and state-owned enterprise dominance in strategic sectors. Not an agent but a structural victim — the coordination mechanism this constraint displaces. Treated as a stakeholder for analytical completeness (agent=false).
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocation, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).

% Non-champion tech startups and SMEs outside the anointed strategic sectors. They face capital discrimination (banks directed to lend to champions), talent poaching by state-backed firms, and procurement exclusion. Exit is trapped: the domestic ecosystem is shaped by the constraint; leaving means exiting the market entirely or relocating abroad.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, private_sme_tech_firms, payer,
    powerless, biographical, trapped, national).

% Households bear the opportunity cost of high investment/low consumption: suppressed wage share, limited financial returns, restricted product variety, and inflated prices in protected sectors. Exit is constrained: emigration is possible for some but most are bound by family, language, and assets. The constraint extracts from them diffusely via the macroeconomic structure.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, household_consumption, payer,
    powerless, biographical, constrained, national).

% Foreign firms and governments facing export controls, entity lists, investment screening, and reciprocal market access restrictions. They would object to the constraint's extraterritorial reach but are structurally excluded from the domestic legitimacy calculus. Their exit is mobile — they can decouple, retaliate, or build alternative supply chains.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, international_trading_partners, excluded,
    powerful, biographical, mobile, global).

% Analyze whether the directed-investment model achieves technological catch-up or generates rent-seeking. They observe from outside the constraint's enforcement, providing the analytical seat. Their exit is analytical — they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, development_economists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of late-development technological catch-up in strategic industries: coordinating massive, patient, risk-tolerant capital across R&D, manufacturing, standards, and supply chains that private markets underprovide due to uncertainty, spillovers, and long horizons.
% TRANSFER_FUNCTION: Moves capital (directed credit, fiscal subsidies), talent (educational pipeline, recruitment mandates), and market access (procurement preferences, entry barriers) from consumer sectors, market allocation, and private SMEs to national champions and defense-tech sectors.
% ABSENT_VOICES: Consumers, private SMEs, and foreign trading partners are structurally excluded from the legitimacy calculus. The constraint's authority derives from national security framing, not democratic deliberation. Their objections appear in trade disputes, capital flight, and brain drain — not in the constraint's internal decision process.
% DISAPPEARANCE_RATIONALE: If the techno-nationalist legitimacy constraint vanished overnight, directed credit would revert to commercial criteria, entity lists would lapse, procurement mandates would expire, and capital would flow to consumer sectors and high-return private ventures. The strategic industrial base would face a financing cliff; national champions would lose protected rents; the state planning apparatus would lose its primary mission. The political economy would reorganize around market allocation — but with massive transition costs in strategic sectors.
% FOUNDING_PROBLEM: Historical technological backwardness leaving the nation vulnerable to foreign coercion, blockade, and great-power subordination — the 'century of humiliation' narrative where lack of strategic industrial capacity meant inability to defend sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: State planners and national champions attest the founding problem remains live: semiconductor choke-points, jet engine gaps, and AI compute dependence prove strategic vulnerability persists. Private economists and trading partners attest partial catch-up has occurred in some domains (telecom, high-speed rail, EVs) and the arrangement now primarily reproduces incumbent rents. Independent economic historians note the founding problem was real but argue the constraint has expanded beyond its original scope (mission creep).
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness rises from 0.52 to 0.78 over the interval as strategic sectors mature into rent-generating incumbents while the security justification expands to cover more industries. Suppression requirement grows from 0.55 to 0.72 as the enforcement apparatus (entity lists, investment screening, capital controls) hardens and broadens. Theater ratio increases from 0.22 to 0.41 as performative 'self-reliance' campaigns and showcase projects absorb a growing share of directed resources relative to actual frontier innovation. Accessibility collapse at 0.68 reflects that alternative allocation mechanisms (market pricing, private venture capital, consumer sovereignty) are structurally blocked in strategic domains. Resistance at 0.54 captures pushback from private tech firms, consumer sectors, and trading partners — real but constrained by the national security framing.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (state planners), the constraint is a rope: genuine coordination of strategic catch-up against market failure. From the national champion seat, it approaches a snare: protected rents with minimal competitive discipline. From the private SME and consumer seats, it is a snare: extraction without voice or exit. The engine computes these seat-level types from the structural data; the authored claim (tangled_rope) captures the hybrid reality that the coordination function is real but the extraction is asymmetric and growing.
 *
 * DIRECTIONALITY LOGIC:
 *   National champions and defense-tech sectors are structural beneficiaries (d ~0.15): they collect directed credit, protected markets, and procurement rents. State planning agencies are agenda-setters (d ~0.10): they administer the constraint and capture bureaucratic authority. Consumer sectors, market-driven allocation, and private SME tech firms are targets (d ~0.85): they face capital starvation, market exclusion, and regulatory burden. Household consumption is a diffuse victim (d ~0.75): suppressed wage share and consumption opportunities are the flip side of high investment rates. The directionality derivation from beneficiary/victim declarations plus exit options (trapped for private SMEs, constrained for consumers, arbitrage for state agencies) produces this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (strategic technological backwardness threatening national survival) was live in the early interval. By the late interval, partial catch-up in some domains (e.g., telecom, high-speed rail) coexists with persistent gaps in others (semiconductors, jet engines). The mandate has not atrophied — the frontier moves — but the extraction-to-coordination ratio has shifted toward extraction in mature strategic sectors. This is a tangled_rope trending toward snare in sub-domains where national champions have become incumbents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the performance_legitimacy kernel, or an independent constraint?',
    'Compare structural beneficiary/victim sets and extractiveness profiles across all four declared readings; if they partition the political economy into different winner/loser sets with different enforcement logics, they are distinct constraints from a shared kernel.',
    'Confirms the committer frame: each reading instantiates a different constraint with its own ε, stakeholders, and classification. The kernel is the contested commitment; the readings are the constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the techno_nationalist_reading of the performance_legitimacy kernel. Sibling readings: quantitative_growth_reading, qualitative_development_reading, livelihood_security_reading.').

omega_variable(
    strategic_necessity_vs_rent_seeking,
    'Does the massive directed investment toward strategic industries reflect genuine security necessity or rent-seeking by national champions?',
    'Track whether strategic sectors achieve global competitiveness without permanent subsidy, or whether protection and direction persist after technological parity is reached. Compare TFP growth in directed vs. non-directed sectors over the interval.',
    'If rent-seeking dominates, the constraint is a snare disguised as coordination. If genuine catch-up occurs, the coordination function is real and the extraction is the price of late-development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_necessity_vs_rent_seeking, empirical, 'Whether the coordination function (technological catch-up for security) is genuine or a cover for extraction.').

omega_variable(
    consumer_suppression_mechanism,
    'Is the suppression of consumer sectors and market allocation structural (policy barriers, capital controls) or internalized (ideological acceptance of sacrifice for national greatness)?',
    'Post-policy relaxation observation: if consumption share rebounds rapidly when constraints ease, suppression was primarily structural. If consumption patterns remain depressed due to internalized frugality or nationalistic consumption norms, internalized component is significant.',
    'If internalized, effective suppression is higher than policy measures suggest — the constraint persists in preferences after formal barriers lift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_suppression_mechanism, conceptual, 'Structural vs. internalized suppression of market-driven allocation and consumer welfare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t6, performance_legitimacy__techno_nationalist_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__techno_nationalist_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(perf_tr_t18, performance_legitimacy__techno_nationalist_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__techno_nationalist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__techno_nationalist_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(perf_be_t6, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(perf_be_t18, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 18, 0.71).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(perf_su_t6, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(perf_su_t18, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, export_control_regime).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, directed_credit_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, entity_list_enforcement).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four constraint stories, each a distinct reading with its own ε, stakeholder set, and classification. This story (techno_nationalist_reading) is the most extractive and enforcement-heavy, prioritizing strategic sectors over consumption. The quantitative_growth_reading is a rope (coordination around growth targeting). The qualitative_development_reading is a scaffold (transitional toward new growth model). The livelihood_security_reading is a rope with snare elements (coordination of welfare delivery with fiscal extraction). All four are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, institutional, 0.1).
constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, organized, 0.2).
constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, moderate, 0.75).
constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
