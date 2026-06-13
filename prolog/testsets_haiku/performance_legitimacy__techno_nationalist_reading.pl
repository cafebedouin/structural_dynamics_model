% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Legitimacy Constraint (Performance Legitimacy Kernel)
 *   domain: political_economy/state_capitalism/development_planning
 *
 * SUMMARY:
 *   The state's legitimacy is grounded in delivering technological
 *   self-sufficiency and global leadership in strategic industries
 *   (semiconductors, advanced robotics, aerospace, quantum computing, battery
 *   technology) to ensure national security and great-power status. This is
 *   ONE READING of the 'performance legitimacy' kernel — a contested claim
 *   about what performance legitimacy actually means. The sibling readings
 *   (quantitative growth, qualitative development, livelihood security)
 *   compete with the techno-nationalist reading for primacy in state policy.
 *   The kernel contest is not resolved by evidence; different parties hold
 *   different readings as authoritative. The techno-nationalist reading
 *   extracts substantially from consumer sectors and lower-income
 *   populations, is actively enforced through capital allocation controls and
 *   export restrictions, and shows increasing theater (state claims success
 *   while actual technological self-sufficiency advances more slowly than
 *   claimed; enforcement machinery is increasingly ceremonial). The
 *   constraint family structure: this reading's core premise (strategic
 *   dominance is the primary legitimacy objective) both coexists with and
 *   forecloses elements of sibling readings — it is incoherent with
 *   livelihood security as the primary measure, but compatible with
 *   quantitative growth and qualitative development as secondary objectives
 *   subordinated to strategic dominance.
 *
 * KEY AGENTS:
 *   - central_planning_authority: sets and enforces the constraint through capital allocation and export controls; gains from maintaining legitimacy through strategic achievement
 *   - strategic_tech_sectors & defense_adjacent_national_champions: primary beneficiaries; capture rents from state protection and directed investment; also carry state direction to achieve impossible-without-subsidy goals
 *   - consumer_goods_sectors: primary victims; lose capital access and market share; decline is politically acceptable because presented as necessary sacrifice
 *   - lower_income_populations: diffuse victims; experience rising consumer prices, redirected wage growth toward strategic sectors, reduced public services; have no formal voice in policy
 *   - international suppliers & export market actors: excluded; face restrictions and cannot reverse exclusion; experience higher prices and uncertain supply on global markets
 *   - research_institutes: held in state direction via identity-lock (patriotic obligation) and institutional dependence; cannot openly challenge research agendas
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.72).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Legitimacy Constraint (Performance Legitimacy Kernel)").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/state_capitalism/development_planning").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '0b6c314b-bdbd-4838-9597-c59adc7214fc').
narrative_ontology:cs_kernel_codification('0b6c314b-bdbd-4838-9597-c59adc7214fc', distributed).
narrative_ontology:cs_authority_grounding('0b6c314b-bdbd-4838-9597-c59adc7214fc', extraction).
narrative_ontology:cs_interpretation_layer_present('0b6c314b-bdbd-4838-9597-c59adc7214fc').
narrative_ontology:cs_reading_relation('0b6c314b-bdbd-4838-9597-c59adc7214fc', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b6c314b-bdbd-4838-9597-c59adc7214fc', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b6c314b-bdbd-4838-9597-c59adc7214fc', performance_legitimacy__livelihood_security_reading, forecloses).
narrative_ontology:cs_axiom('0b6c314b-bdbd-4838-9597-c59adc7214fc', foundational, strategic_dominance_is_primary_legitimacy_measure).
narrative_ontology:cs_axiom_status(strategic_dominance_is_primary_legitimacy_measure, holdable).
narrative_ontology:cs_axiom_grounding('0b6c314b-bdbd-4838-9597-c59adc7214fc', strategic_dominance_is_primary_legitimacy_measure, deontological).
narrative_ontology:cs_axiom('0b6c314b-bdbd-4838-9597-c59adc7214fc', foundational, great_power_status_requires_technological_self_sufficiency).
narrative_ontology:cs_axiom_status(great_power_status_requires_technological_self_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('0b6c314b-bdbd-4838-9597-c59adc7214fc', great_power_status_requires_technological_self_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('0b6c314b-bdbd-4838-9597-c59adc7214fc', technological_vulnerability_security_threat).
narrative_ontology:cs_drift_state('0b6c314b-bdbd-4838-9597-c59adc7214fc', contemporary_supply_chain_resilience_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0b6c314b-bdbd-4838-9597-c59adc7214fc', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, strategic_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_national_champions).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocation_mechanisms).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, lower_income_populations_via_consumption_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, research_institutes_and_universities).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, defense_adjacent_national_champions).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, lower_income_populations).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, research_institutes_and_universities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets five-year plans and industrial policy targeting semiconductor, advanced robotics, aerospace, quantum computing, and battery technology dominance. Controls capital allocation to state-owned enterprises in these sectors and manages export restrictions on dual-use technology. Justifies massive investment as necessary for national security and great-power status. Administers the constraint by directing credit, talent, and raw materials toward strategic industries regardless of consumer-sector demand or market pricing signals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, central_planning_authority, agenda_setter,
    institutional, generational, analytical, national).

% Receive preferential access to capital, subsidized inputs, protected domestic markets, and state-sponsored R&D. Are shielded from price competition and market-discipline restructuring. Can pursue long-term technology leadership projects (10–20 year horizons) that would not survive commercial funding windows. Their executives move between state sector and private roles; the boundary is fluid. Capture rents through protected market access and state largesse.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, strategic_tech_sectors, beneficiary,
    institutional, generational, arbitrage, national).

% Major state-owned enterprises (SOEs) in aerospace, defense manufacturing, telecommunications infrastructure. Benefit from preferential government contracts, technology transfer from research institutes, and de facto monopoly positions. Also bear state expectations to execute industrial policy goals (build supply chain redundancy, maintain employment in politically sensitive regions, develop indigenous technologies regardless of cost). Sit at the boundary between beneficiary (capture rents, control markets) and payer (carry state direction that may conflict with profit maximization).
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_national_champions, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, defense_adjacent_national_champions, payer).

% Textiles, appliances, food processing, retail goods manufacturing. Lose access to credit as capital flows to strategic sectors; face rising input costs from restricted raw materials; cannot compete with subsidized strategic competitors for talent and manufacturing capacity. Shrink in share of GDP and employment even as aggregate growth persists. Their decline is politically acceptable because it is presented as necessary sacrifice for national strength.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors, payer,
    moderate, biographical, constrained, national).

% Experience rising costs for consumer goods (shrinking supply, rising prices as consumer sectors decline); wage growth is redirected toward strategic-sector workers (engineers, technicians); public services and healthcare face budget pressure as state spending concentrates on industrial investment. Are told the sacrifice is temporary and necessary for national greatness. Have no formal seat in industrial policy decisions and cannot exit the domestic market.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, lower_income_populations, payer,
    powerless, immediate, trapped, national).

% International buyers of technology, semiconductors, manufacturing equipment. Face export restrictions, dual-use licensing delays, and unpredictable supply cuts when strategic inventory decisions override commercial delivery. Are structurally excluded from the constraint's governance but experience its extraction (paying higher prices on open markets as supply tightens; losing reliable supply relationships). Cannot object formally because they have no standing in domestic policy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, export_market_actors, excluded,
    moderate, biographical, constrained, global).

% Receive substantial state research funding, access to state-of-the-art facilities, and prestige from contributing to national technological missions. Are also subject to state direction of research agendas away from basic science and toward immediate strategic application. Face restrictions on international collaboration and data sharing. Researchers' mobility and career prospects depend on alignment with state technological priorities. Carry identity-lock through patriotic obligation and institutional dependence.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, research_institutes_and_universities, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, research_institutes_and_universities, payer).

% Western and allied semiconductor makers, software firms, advanced manufacturing equipment vendors. Face restrictions on selling to the polity, technology transfer requirements, and state pressure to share intellectual property. Their exclusion from the market is part of the constraint's function (self-sufficiency requires domestic supply even at high cost). They have economic leverage but no seat in policy decisions and cannot reverse the exclusion unilaterally.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, international_technology_suppliers, excluded,
    powerful, biographical, constrained, global).

% Analytical seat representing alternative readings of performance legitimacy: quantitative growth emphasizes GDP expansion over strategic dominance; qualitative development emphasizes sustainability and efficiency; livelihood security emphasizes daily-life improvements over great-power status. These readings compete with the techno-nationalist reading for primacy in state rhetoric and policy, creating contestation over what performance legitimacy actually means.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, competing_developmental_narratives, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__techno_nationalist_reading, competing_developmental_narratives).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without state coordination of R&D investment and market protection for strategic sectors, individual firms would under-invest in long-horizon technology projects that generate positive externalities (security, supply-chain resilience, technological capability) but do not return profits in market-competition windows. The constraint coordinates sustained investment in strategic capabilities at a scope that individual commercial actors cannot maintain.
% TRANSFER_FUNCTION: Moves resources (capital, talent, raw materials, state credit) from consumer sectors and lower-income populations to strategic tech sectors and defense-adjacent national champions. Moves rents from open-market allocation to state-directed allocation. Moves decision-making authority from commercial pricing to state industrial policy. The transfer persists through export restrictions (closing external alternatives) and credit allocation (constraining internal alternatives).
% ABSENT_VOICES: Export market actors and international suppliers are structurally excluded — they would argue for open trade and competitive pricing but have no formal participation in domestic policy and face restrictions designed to keep them out. Consumer-sector workers and lower-income populations are nominally included (they vote, pay taxes) but have no effective voice in industrial policy decisions that reshape their economic situation; their objections are reframed as 'short-term thinking' or 'lack of strategic vision.' Academic and scientific communities that might champion basic-research agendas over applied technology dominance are subject to state direction and cannot openly contest the agenda.
% DISAPPEARANCE_RATIONALE: If the techno-nationalist legitimacy constraint and its enforcement machinery vanished overnight, capital would reflow to consumer sectors and market-competitive allocation; technology imports would resume from international suppliers; research agendas would decentralize toward basic science and international collaboration; consumer prices would fall; wage structures would equilibrate away from strategic-sector premiums. The polity would reorganize around an alternative performance legitimacy reading — growth, livelihood, or development — or would shift to a hybrid model. The immediate rearrangement would be profound: strategic sectors would face real market discipline, industrial policy would be partially dismantled, and the state's claim to legitimacy would pivot to measurable improvements in daily life rather than great-power status.
% FOUNDING_PROBLEM: In the 1980s–1990s, rapid technological obsolescence in key defense and infrastructure sectors left the polity vulnerable to embargoes and supply disruptions; commercial markets did not provide sufficient investment in dual-use technologies that were strategically critical but commercially marginal. Without state coordination, the polity faced the prospect of technological dependence on rivals, which threatened both security and status.
% FOUNDING_PROBLEM_CORROBORATION: The central planning authority and strategic tech sectors attest the founding problem is still live — technology competition is intensifying, rivals are advancing faster, and supply-chain vulnerabilities persist. Economists and development analysts outside the benefiting parties attest the founding problem was substantially resolved by the 2010s; supply chains have diversified, commercial investment in dual-use technology has expanded, and the constraint now persists primarily as a legitimacy mechanism rather than a security necessity. International technology suppliers and export analysts attest the founding problem was real but is now a cover story — the constraint is maintained because it concentrates power and rents, not because external vulnerability remains critical.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).

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
 *   Extractiveness starts at 0.54 (genuine coordination function present — long-horizon R&D investment that markets undersupply) but rises to 0.68 as the constraint ages and founding-problem justifications weaken relative to rent-capture behavior. Suppression rises from 0.58 to 0.72 because enforcement machinery becomes more sophisticated and more explicitly coercive (export controls harden, internal compliance monitoring expands, and capital controls tighten). Theater rises from 0.32 to 0.48 because state rhetoric increasingly emphasizes victory and dominance while actual technological leadership lags behind claims — the gap widens and more enforcement effort goes to maintaining the legitimacy claim than to delivering results. Accessibility collapse is moderate (0.62): alternatives exist (technology import, commercial allocation, market pricing) but are suppressed; the suppression is not as complete as a natural law or as diffuse as a snare, because the constraint rides on a state apparatus that could change policy. Resistance is substantial (0.58): consumers and export markets push back; research communities privately contest; some SOE managers perceive the direction as counterproductive. The one-shared-time-grid discipline ensures every metric is valued at every measurement point, with shared endpoints so no metric can drift ahead or behind through absent measurements.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute vastly different types: from the planning-authority seat, the constraint is genuine tangled-rope coordination (real problem, asymmetric but justified). From the consumer and lower-income seats, it is snare (pure extraction under the guise of security; exit is impossible; alternatives are suppressed; beneficiaries capture, payers lose). The research seat computes as intermediate (rope with identity-lock — genuine intellectual contribution, but autonomy is constrained and the lock is identity-based, not structural). No one seat's computation is 'wrong' — the constraint's structure genuinely produces different effective types depending on positionality. This perspectival divergence is THE measurement the engine exists to take: a constraint that the center claims is coordination and the periphery experiences as extraction is exactly how institutional extraction works.
 *
 * DIRECTIONALITY LOGIC:
 *   Central planning authority: d ≈ 0.1 (powerful, analytical, sets the rules, captures legitimacy and credit for achievement). Strategic tech sectors & national champions: d ≈ 0.15–0.25 (institutional power, constrained but subsidized exit, clear beneficiaries). Consumer sectors: d ≈ 0.70 (moderate power, constrained exit, pay through lost capital and competitive disadvantage). Lower-income populations: d ≈ 0.85 (powerless, trapped, pay through inflation and wage redirection, cannot exit or voice objection). Research communities: d ≈ 0.55 (moderate power but identity-locked; some benefit from research prestige and funding, but autonomy is constrained). International suppliers: d ≈ 1.0 (excluded, powerful but trapped by design, have no entry point and their exclusion is the goal).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1980s–90s technological vulnerability and supply disruption risk) has substantially attenuated by the 2010s — supply chains diversified, international tech investment expanded, and dual-use technology became commercial in many sectors. The constraint persists because it has transitioned from a solution to a legitimacy mechanism: the state now needs technological dominance as a performance claim to ground its authority, rather than to solve a critical security gap. This is the classical mandatrophy signature: the founding mandate (ensure strategic security through technological self-sufficiency) is dead or solved, but the constraint persists as an administrative mechanism. The theater ratio rising to 0.48 is the mandatrophy marker — increasing proportion of enforcement effort goes to maintaining the legitimacy claim (announcing achievements, suppressing evidence of lag) rather than to solving problems. The constraint is entering piton territory: it extracts substantially but does not solve the problem it was built for; the founding-problem status is contested precisely because different parties disagree whether the problem is live or solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is technological vulnerability still a critical founding problem, or has supply-chain diversification and commercial tech investment substantially solved it?',
    'Independent assessment of supply-chain resilience, dual-use technology availability, and strategic dependency scenarios by analysts outside the benefiting parties.',
    'If the founding problem is substantially solved, the constraint is mandatrophic — it persists as legitimacy mechanism and rent-collection apparatus, not as a solution. Classification shifts toward piton (extractive, inertial, performing coordination that is no longer functionally necessary). If the founding problem is still live, the constraint is genuine tangled-rope coordination with asymmetric extraction justified by necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding security problem has been solved or merely transitioned to a legitimacy claim.').

omega_variable(
    kernel_reading_priority,
    'Which reading of performance legitimacy is actually PRIMARY in state decision-making — techno-nationalist dominance, quantitative growth, qualitative development, or livelihood security?',
    'Examine budget allocation sequences, policy reversals when readings conflict, and which objectives are sacrificed when trade-offs force choice. Track what gets cut when capital is scarce.',
    'If techno-nationalist reading is genuinely primary, the constraint''s type and metrics stand. If another reading is actually primary and techno-nationalism is instrumental to it, the constraint''s ε and beneficiary structure change — strategic sectors are secondary beneficiaries, not primary ones; the real beneficiary is whichever reading is actually driving allocations. The entire constraint classification could shift depending on which reading is operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_priority, conceptual, 'Which of the four sibling readings is actually primary in state decision-making, and which are rationalization.').

omega_variable(
    strategic_sector_definition_boundary,
    'What determines which sectors are counted as ''strategic'' and therefore entitled to state protection and investment?',
    'Examine criteria for strategic designation, track sectors that transition between strategic and non-strategic, and measure how designation decisions are made (formal methodology, political influence, capability assessment, or opaque discretion).',
    'If the boundary is clear and capability-based, the constraint is structurally coherent (investment follows from strategic need). If the boundary is opaque or politically driven, sectors may capture ''strategic'' status for rent-seeking purposes unrelated to actual strategic value — the constraint becomes a cover for industrial patronage rather than security investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_sector_definition_boundary, empirical, 'Whether ''strategic sector'' status is justified by capability and security need or by political capture.').

omega_variable(
    identity_lock_mechanism_research_communities,
    'For research institutes and universities, how much of the suppression and direction is structural (institutional dependence on state funding, career advancement gated by alignment) versus internalized (researchers genuinely believe patriotic obligation or accept the direction as legitimate)?',
    'Track researcher mobility and dissent when funding is decoupled from state control; examine private versus public statements; measure research agenda shifts when direction changes or when researchers move to jurisdictions with different constraints.',
    'If suppression is primarily structural, it would ease if state control were reduced. If it is primarily internalized, the identity-lock persists even after structural constraints are removed — researchers carry the frame with them. This determines whether the constraint''s effective suppression declines if institutional independence increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_research_communities, empirical, 'Whether research-community direction is structural or internalized identity-lock.').

omega_variable(
    kernel_contest_stability,
    'Is the kernel contest between performance-legitimacy readings stable, or are there structural forces that would drive the polity toward one reading at the expense of others?',
    'Model feedback loops: if techno-nationalist reading drives capital allocation toward strategic sectors, growth accrues there and not in consumer sectors, which may make livelihood security harder to deliver and shift contestation. Track which reading''s legitimacy claim faces falsification pressure as conditions change.',
    'If the contest is stable, all four readings remain live and contestable. If structural forces drive toward one reading (or away from it), the constraint may transition to monopoly or marginality. The reading-relations classification (coexists_with vs. forecloses) depends on whether the kernel contest is genuinely multi-polar or whether one reading is unstably dominant and will eventually eliminate others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_stability, conceptual, 'Whether the performance-legitimacy kernel contest is stable or driven toward closure by structural forces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(perf_tr_t0, observed).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__techno_nationalist_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(perf_tr_t5, observed).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__techno_nationalist_reading, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(perf_tr_t10, observed).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__techno_nationalist_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(perf_tr_t15, observed).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(perf_tr_t20, observed).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__techno_nationalist_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(perf_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(perf_be_t0, observed).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(perf_be_t5, observed).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(perf_be_t10, observed).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(perf_be_t15, observed).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(perf_be_t20, observed).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(perf_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(perf_su_t0, observed).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(perf_su_t5, observed).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(perf_su_t10, observed).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(perf_su_t15, observed).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(perf_su_t20, observed).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(perf_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, trade_policy__export_control_regime).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, institutional_identity__research_community_direction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel. The sibling readings (quantitative_growth, qualitative_development, livelihood_security) are separate constraint stories, each with its own ε, beneficiary/victim structure, and classified type. These readings are not perspectives on a single constraint — they are structurally distinct constraints that compete for primacy in state policy. The kernel contest is unresolved and ongoing; no reading has achieved definitional closure. All four readings are authored as separate stories and linked via network.affects_constraints. The choice of reading shapes which sectors benefit, which populations bear costs, and how extraction is justified. This techno-nationalist reading makes strategic dominance the primary legitimacy measure; sibling readings invert this priority. The readings coexist as live positions held by different state agencies, constituencies, and policy camps. None forecloses the others in principle, though in practice the techno-nationalist reading currently dominates five-year planning and industrial policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
