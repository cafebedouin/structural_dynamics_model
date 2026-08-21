% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Techno-Nationalist Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint represents the 'techno-nationalist' reading of
 *   performance legitimacy, where the state's authority is primarily grounded
 *   in its ability to achieve technological self-sufficiency and global
 *   leadership in strategic industries. This involves massive directed
 *   investment, protectionist policies, and prioritization of national
 *   champions, often at the expense of market efficiency and consumer
 *   welfare. It is a contested reading within the broader 'performance
 *   legitimacy' kernel, emphasizing national security and great-power status
 *   over other development goals.
 *
 * KEY AGENTS:
 *   - state_planning_agencies: Agenda setter (institutional/identity_locked)
 *   - defense_adjacent_tech_sectors: Primary beneficiary (organized/mobile)
 *   - national_champions_enterprises: Secondary beneficiary (powerful/constrained)
 *   - consumer_sectors: Primary payer (moderate/constrained)
 *   - market_driven_allocation: Victim (powerless/trapped)
 *   - foreign_tech_providers: Excluded (institutional/constrained)
 *   - citizens_as_consumers: Payer (powerless/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.7).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.85).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '6d856048-884c-40f3-85f1-6deed252ed7a').
narrative_ontology:cs_kernel_codification('6d856048-884c-40f3-85f1-6deed252ed7a', formalized).
narrative_ontology:cs_authority_grounding('6d856048-884c-40f3-85f1-6deed252ed7a', extraction).
narrative_ontology:cs_interpretation_layer_present('6d856048-884c-40f3-85f1-6deed252ed7a').
narrative_ontology:cs_reading_relation('6d856048-884c-40f3-85f1-6deed252ed7a', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('6d856048-884c-40f3-85f1-6deed252ed7a', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('6d856048-884c-40f3-85f1-6deed252ed7a', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('6d856048-884c-40f3-85f1-6deed252ed7a', foundational, technological_sovereignty_is_national_security).
narrative_ontology:cs_axiom_status(technological_sovereignty_is_national_security, holdable).
narrative_ontology:cs_axiom_grounding('6d856048-884c-40f3-85f1-6deed252ed7a', technological_sovereignty_is_national_security, deontological).
narrative_ontology:cs_axiom('6d856048-884c-40f3-85f1-6deed252ed7a', foundational, state_directed_investment_ensures_strategic_advantage).
narrative_ontology:cs_axiom_status(state_directed_investment_ensures_strategic_advantage, holdable).
narrative_ontology:cs_axiom_grounding('6d856048-884c-40f3-85f1-6deed252ed7a', state_directed_investment_ensures_strategic_advantage, empirically_contingent).
narrative_ontology:cs_reference_frame('6d856048-884c-40f3-85f1-6deed252ed7a', great_power_technological_competition_framework).
narrative_ontology:cs_drift_state('6d856048-884c-40f3-85f1-6deed252ed7a', contemporary_global_supply_chain_disruptions, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6d856048-884c-40f3-85f1-6deed252ed7a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champions_enterprises).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_planning_agencies).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, foreign_tech_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, citizens_as_consumers).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, national_security_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, great_power_competition_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs massive state investment into strategic industries, sets R&D priorities, and implements industrial policies to achieve technological self-sufficiency. Their legitimacy is tied to the success of these long-term national projects.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_planning_agencies, agenda_setter,
    institutional, generational, identity_locked, national).

% Receives substantial state subsidies, preferential policies, and protected market access. Their growth is prioritized over market efficiency, and they are expected to deliver breakthroughs in critical technologies.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    organized, biographical, mobile, national).

% Large state-backed or state-aligned enterprises that are designated to lead in strategic sectors. They benefit from state support and market protection but are also subject to state direction and performance mandates.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champions_enterprises, beneficiary,
    powerful, biographical, constrained, global).

% Faces resource diversion, higher costs due to protectionist policies, and potentially less innovation in non-strategic areas. Their needs are secondary to the national strategic goals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sectors, payer,
    moderate, immediate, constrained, national).

% The principle of allocating resources based purely on supply and demand is overridden by state industrial policy. Capital and talent are directed to strategic sectors regardless of market signals, leading to inefficiencies in other areas.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocation, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__techno_nationalist_reading, market_driven_allocation).

% Faces market access barriers, forced technology transfer, and export controls designed to protect and advance domestic strategic industries. They are actively suppressed from competing in key sectors.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, foreign_tech_providers, excluded,
    institutional, generational, constrained, global).

% Bear the costs of protectionism through higher prices, limited choices, and potentially lower quality goods in non-strategic sectors, as resources are diverted to national champions. Their direct economic welfare is secondary to national strategic goals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, citizens_as_consumers, payer,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, state_planning_agencies).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national resources (capital, talent, policy) towards a unified goal of technological self-sufficiency and global leadership in strategic industries, preventing fragmented efforts and ensuring alignment with national security objectives.
% TRANSFER_FUNCTION: Transfers massive state capital, preferential market access, and policy support from the general economy (consumers, non-strategic sectors) to defense-adjacent tech sectors and national champions.
% ABSENT_VOICES: Advocates for pure market liberalization, consumer choice, and international economic integration are largely excluded from policy-making, as their arguments conflict with the techno-nationalist imperative. Foreign tech providers are actively shut out.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the state's industrial policy would collapse, leading to a reallocation of capital and talent based on market signals. Strategic industries would lose their protected status and subsidies, likely leading to significant restructuring or failure for some 'national champions'. Consumer sectors would benefit from increased competition and resource availability. The national security posture would be fundamentally altered.
% FOUNDING_PROBLEM: The nation faced perceived vulnerabilities in critical technologies, dependence on foreign suppliers, and a desire to achieve great-power status through technological dominance, particularly in defense and advanced manufacturing.
% FOUNDING_PROBLEM_CORROBORATION: National security analysts, defense strategists, and state-affiliated think tanks consistently corroborate the live status of the founding problem, citing ongoing geopolitical competition and technological rivalries. This is largely attested by actors within the state apparatus and its aligned institutions, with limited independent corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates national resources towards a strategic goal (technological leadership) but does so through highly extractive and suppressive means. Extraction is high (0.7) due to the diversion of resources and market distortions. Suppression is very high (0.85) as it requires active state intervention to override market signals, exclude foreign competition, and direct private capital. Theater ratio is low (0.2) because the state's commitment to achieving these strategic goals is genuine and the efforts are largely functional, not performative, even if inefficient. Accessibility collapse is high (0.75) as market-based alternatives for resource allocation are actively suppressed. Resistance is moderate (0.4) from affected sectors and consumers, but it is largely contained by the state's institutional power and the framing of national interest.
 *
 * PERSPECTIVAL GAP:
 *   State planning agencies and beneficiaries in strategic sectors perceive this as a necessary and effective coordination mechanism for national survival and prosperity. Conversely, consumer sectors and proponents of market-driven allocation experience it as a highly extractive and suppressive force that distorts the economy and imposes costs on the populace. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning agencies and defense-adjacent tech sectors are clear beneficiaries, receiving directed resources and protection. Consumer sectors and the abstract 'market-driven allocation' are victims, bearing the costs of resource diversion and market distortion. Foreign tech providers are actively excluded, making them targets of suppression. Citizens as consumers bear indirect costs, placing them as payers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring extraction) or a pure Snare (ignoring the genuine coordination function for national strategic goals). The 'live' status of the founding problem, combined with high extraction and suppression, indicates an actively maintained Tangled Rope, where the coordination function is intertwined with significant state-directed extraction for strategic ends. The low theater ratio suggests it is not a Piton, as the state genuinely pursues the stated goals, even if inefficiently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_efficiency_vs_strategic_autonomy,
    'What is the true economic cost of prioritizing strategic autonomy over market efficiency, and is this cost sustainable long-term?',
    'Independent economic modeling comparing the techno-nationalist approach with market-driven alternatives, including long-term growth projections and innovation rates.',
    'If costs are found to be prohibitive and unsustainable, the legitimacy of this reading could erode, potentially shifting policy towards more market-oriented approaches or a different reading of performance legitimacy. If costs are deemed acceptable for strategic gains, the constraint''s persistence is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_vs_strategic_autonomy, empirical, 'The trade-off between economic efficiency and national strategic goals.').

omega_variable(
    technological_leadership_metrics,
    'Are the chosen metrics for ''technological self-sufficiency'' and ''global leadership'' genuinely reflective of innovation and strategic advantage, or are they susceptible to ''Goodhart''s Law'' (metrics becoming targets)?',
    'Longitudinal analysis of innovation output, patent quality, and actual market penetration of ''national champions'' versus global benchmarks, independent of state-reported figures.',
    'If metrics are found to be distorted, the theater_ratio could be higher than currently assessed, indicating a shift towards performative rather than functional achievement, potentially reclassifying towards a Piton or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_leadership_metrics, empirical, 'Validity of metrics used to assess technological leadership.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s legitimacy truly derived from technological performance, or is this a post-hoc rationalization for maintaining control and resource allocation to favored sectors?',
    'Sociological studies of public opinion, analysis of state propaganda vs. actual citizen priorities, and historical analysis of regime stability factors beyond economic performance.',
    'If the techno-nationalist narrative is primarily a cover for extraction, the constraint''s effective extractiveness would be higher, and its classification might shift closer to a Snare. If it genuinely resonates with the populace, the coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Ambiguity of the true source of state legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__techno_nationalist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__techno_nationalist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__techno_nationalist_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(perf_tr_t50, performance_legitimacy__techno_nationalist_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(perf_be_t40, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(perf_be_t50, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(perf_su_t50, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'performance_legitimacy' kernel. Each reading emphasizes a different aspect of state performance as the primary source of legitimacy, leading to distinct structural constraints and beneficiary/victim sets. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
