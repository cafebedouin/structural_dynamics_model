% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Techno-Nationalist Performance Legitimacy: Strategic Industry Dominance as National Security Imperative
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint story captures the techno-nationalist reading of the
 *   performance_legitimacy kernel: the state's right to rule is grounded in
 *   achieving technological self-sufficiency and global leadership in
 *   strategic industries as the foundation of national security and
 *   great-power status. The arrangement coordinates massive resource
 *   mobilization toward strategic sectors (semiconductors, AI, quantum,
 *   aerospace) through directed credit, export controls, procurement
 *   mandates, and supply chain resilience directives. It extracts from
 *   consumer sectors, market-driven allocation, and household savings to fund
 *   national champions and defense-adjacent industries. The constraint is
 *   actively enforced — entity lists, investment screening, technology
 *   transfer restrictions, and capital controls maintain the allocation. The
 *   claimed type is tangled_rope: genuine coordination of strategic-depth
 *   investment coexists with asymmetric extraction from non-strategic
 *   sectors. The kernel context identifies this as one of four contested
 *   readings of performance legitimacy, alongside livelihood_security,
 *   qualitative_development, and quantitative_growth readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.72).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.78).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy: Strategic Industry Dominance as National Security Imperative").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '9f472515-c40b-4367-b1bd-e1f275d34479').
narrative_ontology:cs_kernel_codification('9f472515-c40b-4367-b1bd-e1f275d34479', formalized).
narrative_ontology:cs_authority_grounding('9f472515-c40b-4367-b1bd-e1f275d34479', extraction).
narrative_ontology:cs_interpretation_layer_present('9f472515-c40b-4367-b1bd-e1f275d34479').
narrative_ontology:cs_reading_relation('9f472515-c40b-4367-b1bd-e1f275d34479', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_reading_relation('9f472515-c40b-4367-b1bd-e1f275d34479', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f472515-c40b-4367-b1bd-e1f275d34479', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_axiom('9f472515-c40b-4367-b1bd-e1f275d34479', foundational, technological_sovereignty_is_existential).
narrative_ontology:cs_axiom_status(technological_sovereignty_is_existential, holdable).
narrative_ontology:cs_axiom_grounding('9f472515-c40b-4367-b1bd-e1f275d34479', technological_sovereignty_is_existential, deontological).
narrative_ontology:cs_axiom('9f472515-c40b-4367-b1bd-e1f275d34479', foundational, strategic_sectors_justify_market_distortion).
narrative_ontology:cs_axiom_status(strategic_sectors_justify_market_distortion, holdable).
narrative_ontology:cs_axiom_grounding('9f472515-c40b-4367-b1bd-e1f275d34479', strategic_sectors_justify_market_distortion, instrumental).
narrative_ontology:cs_axiom('9f472515-c40b-4367-b1bd-e1f275d34479', secondary, great_power_status_requires_indigenous_innovation).
narrative_ontology:cs_axiom_status(great_power_status_requires_indigenous_innovation, holdable).
narrative_ontology:cs_axiom_grounding('9f472515-c40b-4367-b1bd-e1f275d34479', great_power_status_requires_indigenous_innovation, conventional).
narrative_ontology:cs_reference_frame('9f472515-c40b-4367-b1bd-e1f275d34479', revolutionary_technological_breakthrough).
narrative_ontology:cs_drift_state('9f472515-c40b-4367-b1bd-e1f275d34479', post_tech_war_escalation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9f472515-c40b-4367-b1bd-e1f275d34479', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_owned_enterprises_strategic).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, military_industrial_complex).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, private_entrepreneurs_non_strategic).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, household_consumers).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, technological_sovereignty_ensures_national_survival).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, state_directed_investment_outperforms_market_in_strategic_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the strategic industrial policy framework: identifies strategic sectors, directs credit and subsidies, imposes export controls, and coordinates supply chain resilience mandates. Legitimacy rests on delivering technological breakthroughs that secure great-power status.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planning_agency, agenda_setter,
    institutional, generational, analytical, national).

% Receive massive directed investment, preferential credit, procurement guarantees, and protection from foreign competition. Semiconductors, AI, quantum, aerospace, advanced materials. Their success is measured by technical milestones, not market profitability. Exit means losing state support and protected markets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    organized, biographical, constrained, national).

% State-owned or state-backed enterprises anointed as global competitors in strategic industries. Receive capital, policy cover, and diplomatic backing to acquire technology and market share abroad. They shape policy through revolving-door personnel and capture of standard-setting bodies. Too big and strategically vital to exit.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champions, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, national_champions, agenda_setter).

% Service, retail, light manufacturing, and non-strategic tech face capital starvation as credit is directed to priority sectors. Labor and land costs rise from strategic-sector demand. Regulatory compliance burdens increase. Exit is difficult — domestic market access depends on political compliance, and foreign markets may be blocked by strategic-sector trade tensions.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sectors, payer,
    moderate, biographical, constrained, national).

% The price mechanism and competitive allocation are structurally suppressed in strategic sectors. Capital flows by administrative directive, not return on investment. Financial intermediaries are mandated to lend to priority projects. The market's coordinating function is displaced where the state declares strategic necessity. No exit — the constraint defines the rules of the game.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocation, payer,
    organized, generational, trapped, national).

% Founders and investors in non-priority sectors face asymmetric access to capital, talent, and policy support. Many migrate to strategic sectors nominally, or relocate capital and talent abroad. Their exclusion from the legitimacy narrative means their success is politically invisible or suspect.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, private_entrepreneurs_non_strategic, payer,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, private_entrepreneurs_non_strategic, excluded).

% Bear the downstream costs: higher prices from protected markets, reduced product variety from import substitution, delayed access to global innovations due to export controls, and opportunity cost of savings channeled into low-return strategic investments. No voice in industrial policy; exit means emigration or political dissent.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, household_consumers, payer,
    powerless, immediate, trapped, local).

% Multilateral export control regimes (Wassenaar, etc.), standards bodies, and trade dispute mechanisms. They constrain the external environment — denying technology transfer, imposing entity lists, shaping global supply chains. Their actions validate the techno-nationalist narrative of external containment, reinforcing the constraint's internal logic.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, international_technology_regime, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes national resources — capital, talent, policy attention — toward technological capabilities that no single firm could internalize: semiconductor supply chains, foundational AI research, quantum communications, aerospace propulsion. Solves the collective-action problem of long-horizon, high-uncertainty, strategic-depth investment where market signals are absent or misleading.
% TRANSFER_FUNCTION: Moves capital (directed credit, fiscal subsidies, below-market land), talent (assignment systems, hukou-linked incentives, military-civil fusion), and market access (procurement preferences, entry barriers for foreign rivals, export control waivers) from consumer sectors, households, and market-driven allocation to defense-adjacent tech sectors and national champions.
% ABSENT_VOICES: Rural populations bearing environmental externalities of strategic industrial clusters; foreign technology partners excluded by indigenous innovation mandates; consumer-citizens who would prefer cheaper goods and services over strategic autonomy; reform-oriented economists within the system whose market-allocation arguments are treated as ideological deviation.
% DISAPPEARANCE_RATIONALE: If the techno-nationalist legitimacy constraint vanished overnight, capital would flood back to consumer and service sectors, exchange rates and interest rates would re-price risk, export control compliance infrastructure would dismantle, and national champions would face immediate solvency crises without state-directed rollover. The entire industrial geography and financial architecture would reorganize around market signals within 2-3 years.
% FOUNDING_PROBLEM: The historical vulnerability to technology denial regimes (COCOM, Wassenaar, entity lists) and the perceived existential risk of dependence on adversary-controlled supply chains for defense-critical technologies — the 'stranglehold' problem that motivated the original push for self-reliance in the 1950s-60s and was reactivated by the US-China tech conflict post-2018.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by declassified defense industry archives and military-strategic literature from the 1950s-70s (outside the current beneficiary set). Contemporary corroboration is split: strategic studies institutes and military journals attest the problem is live and escalating; trade economists and WTO dispute records attest the denial regime has evolved but the indigenous innovation response has generated diminishing returns and new dependencies; the beneficiary sectors (national champions) uniformly attest the problem is live and requires expanded support.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the transfer from non-strategic to strategic sectors is large, persistent, and decoupled from market returns — capital is allocated by strategic priority, not marginal productivity. Suppression (0.78) is higher still because the constraint's persistence depends on actively suppressing market signals (interest rates, exchange rates, entry/exit) and foreign competition in strategic sectors. Theater ratio (0.42) is moderate: genuine technological breakthroughs occur (coordination function), but a growing share of enforcement activity defends rent capture by national champions rather than strategic necessity. Accessibility collapse (0.68) reflects the difficulty of operating outside the state-directed framework in strategic domains. Resistance (0.55) is moderate — consumer sectors and market advocates resist but lack coordinated political voice. The measurement series on a shared grid (2015-2030) shows extractiveness and suppression rising together as the tech conflict escalated, with theater ratio increasing as strategic rhetoric increasingly covers rent-seeking.
 *
 * PERSPECTIVAL GAP:
 *   From the state_planning_agency seat, the constraint is genuine coordination solving a market failure in strategic-depth investment — the engine should compute rope/tangled_rope. From consumer_sectors and household_consumers, the same structure operates as enforced extraction with suppressed alternatives — the engine should compute snare/tangled_rope. From national_champions (dual role), the constraint is both the source of their rents and the source of their political discipline — their computed type depends on whether the coordination benefit outweighs the extraction cost at their seat. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_planning_agenda_setter sits at d ≈ 0.1 (beneficiary end) — it controls the constraint and captures political legitimacy from its operation. Defense_adjacent_tech_sectors and national_champions are beneficiaries at d ≈ 0.2-0.3 — they collect directed resources but face performance mandates and political discipline. Consumer_sectors, market_driven_allocation, and private_entrepreneurs are payers at d ≈ 0.7-0.8 — they bear capital starvation and regulatory burden with constrained exit. Household_consumers are payers at d ≈ 0.9 — trapped, powerless, bearing diffuse downstream costs. International_technology_regime is an observer at d = 0.5 (analytical) — it constrains the external environment but does not participate in the domestic allocation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (technology denial/stranglehold) remains live but has mutated: the original denial regime was broad and crude; today's is targeted and sophisticated (entity lists, end-use controls, investment screening). The techno-nationalist response has generated genuine capabilities (Beidou, high-speed rail, 5G, hypersonics) but also massive overcapacity in legacy strategic sectors (shipbuilding, traditional semiconductors) and rent-seeking by national champions. The mandatrophy tension: the coordination function (solving strategic-depth investment) is real, but the extraction function has expanded beyond what the coordination requires. The constraint persists because the national security narrative immunizes it from cost-benefit review — questioning the allocation is framed as questioning national survival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the techno_nationalist_reading a distinct constraint with its own ε, or a parameterization of a single performance_legitimacy constraint?',
    'Test ε-invariance: if measuring extractiveness against the techno-nationalist reading''s structural referent (strategic sector allocation) yields a different ε than measuring against the livelihood_security referent (household welfare allocation), they are distinct constraints. The ε-invariance principle requires decomposition when observables change ε.',
    'If distinct constraints, each reading gets its own story, stakeholders, and classification — the kernel is a family of constraints linked by network.affects_constraints. If one constraint, the readings are observer perspectives on a single arrangement, and the framework''s per-seat computation handles the divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel''s readings are structurally distinct constraints (per ε-invariance) or perspectival variants of one constraint.').

omega_variable(
    strategic_vs_market_coordination_boundary,
    'How much of the measured extractiveness is the genuine cost of coordinating strategic-depth investment (which markets cannot price), versus rent extraction by national champions using strategic rhetoric?',
    'Counterfactual: if strategic sectors were funded at market rates with only information-sharing coordination (no directed credit, no entry barriers, no export controls), what capability gap would remain? The difference between actual transfer and counterfactual coordination cost is the extractive margin.',
    'A large extractive margin reclassifies toward snare; a small margin supports tangled_rope with genuine coordination core. Affects Boltzmann floor calculation for enforcement_mechanism coordination type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_vs_market_coordination_boundary, empirical, 'The coordination-extraction boundary within the strategic sector transfer.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of market allocation in strategic sectors structural (capital controls, administrative allocation, legal barriers) or internalized (elite belief that market mechanisms are incompatible with strategic survival, career dependence on the directed-investment paradigm)?',
    'Post-liberalization suppression trajectory: if market mechanisms were experimentally permitted in a strategic sub-sector (e.g., commercial satellite launch), would capital and talent flow to market allocation, or would internalized strategic frames maintain directed allocation?',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the target (market_driven_allocation) carries the suppression as cognitive frame. Affects identity_locked dynamics for state_planning_agency and national_champions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in the techno-nationalist allocation framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2015, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(perf_tr_t2018, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2018, 0.32).
narrative_ontology:measurement(perf_tr_t2021, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(perf_tr_t2024, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement(perf_tr_t2027, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2027, 0.42).
narrative_ontology:measurement(perf_tr_t2030, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2030, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t2015, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(perf_be_t2018, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(perf_be_t2021, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement(perf_be_t2024, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2024, 0.71).
narrative_ontology:measurement(perf_be_t2027, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2027, 0.72).
narrative_ontology:measurement(perf_be_t2030, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2030, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2015, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(perf_su_t2018, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement(perf_su_t2021, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2021, 0.74).
narrative_ontology:measurement(perf_su_t2024, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2024, 0.77).
narrative_ontology:measurement(perf_su_t2027, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2027, 0.78).
narrative_ontology:measurement(perf_su_t2030, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2030, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four constraint stories (this reading + three siblings) per the ε-invariance principle: each reading has a different beneficiary/victim structure, different extractiveness profile, and different coordination function. This techno_nationalist_reading prioritizes strategic industry dominance as the binding legitimacy constraint, extracting from consumer sectors and market allocation to fund defense-adjacent tech sectors and national champions. The quantitative_growth_reading is structurally downstream — techno-nationalist investment often sacrifices short-term GDP growth, creating tension. The qualitative_development_reading coexists in tension — strategic sectors may drive innovation but distort efficiency. The livelihood_security_reading is most opposed — resource diversion from consumption to strategic investment directly conflicts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, organized, 0.25).
constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, moderate, 0.75).
constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
