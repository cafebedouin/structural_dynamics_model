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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Techno-Nationalist Performance Legitimacy Regime
 *   domain: economic/political
 *
 * SUMMARY:
 *   A state-capitalist regime grounds its claim to rule in delivering
 *   technological self-sufficiency and leadership in strategic industries.
 *   The operating arrangement is an allocation machine: published
 *   strategic-industry lists decide which firms get below-market state
 *   credit, guaranteed procurement, regulatory forbearance, and protection
 *   from foreign competition; export controls, localization mandates, and
 *   capital controls police the boundary; cadre evaluations tie official
 *   careers to milestone delivery. The security problem the arrangement
 *   addresses is genuine - chokepoint leverage is real and has been exercised
 *   against the regime - and the same machinery concentrates its benefits on
 *   national champions and defense-adjacent sectors while its costs diffuse
 *   across household savers, consumer firms, and non-strategic private
 *   enterprise. Metrics are authored independently of the claim: the story
 *   claims tangled_rope and reports the descriptive metrics separately; the
 *   engine computes per-seat classifications from the structural data.
 *   Epsilon's referent is the standing techno-nationalist allocation
 *   arrangement as it operates, assessed by this reading's own lights.
 *
 * KEY AGENTS:
 *   - central_planning_authority: Agenda-setter (institutional/arbitrage) - defines the strategic lists, directs credit, collects legitimacy from milestone delivery
 *   - strategic_industrial_bureaucracy: Agenda-setter and beneficiary (institutional/identity_locked) - administers the funds; careers fused to the mission
 *   - defense_adjacent_tech_sectors: Primary beneficiary (powerful/mobile) - receives directed capital and protected demand
 *   - national_champions: Dual-positioned beneficiary-payer (powerful/constrained) - absorbs the transfer, carries its policy burdens
 *   - household_savers: Primary payer (moderate/trapped) - finances the buildout through repressed returns under capital controls
 *   - consumer_sector_firms and non_strategic_private_enterprises: Payers (moderate/constrained) - capital-starved outside the lists
 *   - foreign_technology_suppliers: Excluded (institutional/arbitrage) - the enforcement object of the export-control boundary
 *   - development_economists: Analytical observer - sees the full allocation structure no participant seat observes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.71).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.75).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy Regime").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "economic/political").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '5046eb57-8330-491e-9a74-e3cf29fc7eb3').
narrative_ontology:cs_kernel_codification('5046eb57-8330-491e-9a74-e3cf29fc7eb3', formalized).
narrative_ontology:cs_authority_grounding('5046eb57-8330-491e-9a74-e3cf29fc7eb3', extraction).
narrative_ontology:cs_interpretation_layer_present('5046eb57-8330-491e-9a74-e3cf29fc7eb3').
narrative_ontology:cs_reading_relation('5046eb57-8330-491e-9a74-e3cf29fc7eb3', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('5046eb57-8330-491e-9a74-e3cf29fc7eb3', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('5046eb57-8330-491e-9a74-e3cf29fc7eb3', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('5046eb57-8330-491e-9a74-e3cf29fc7eb3', foundational, technological_self_sufficiency_is_existential).
narrative_ontology:cs_axiom_status(technological_self_sufficiency_is_existential, holdable).
narrative_ontology:cs_axiom_grounding('5046eb57-8330-491e-9a74-e3cf29fc7eb3', technological_self_sufficiency_is_existential, empirically_contingent).
narrative_ontology:cs_axiom('5046eb57-8330-491e-9a74-e3cf29fc7eb3', foundational, great_power_status_requires_strategic_dominance).
narrative_ontology:cs_axiom_status(great_power_status_requires_strategic_dominance, holdable).
narrative_ontology:cs_axiom_grounding('5046eb57-8330-491e-9a74-e3cf29fc7eb3', great_power_status_requires_strategic_dominance, conventional).
narrative_ontology:cs_axiom('5046eb57-8330-491e-9a74-e3cf29fc7eb3', secondary, allocation_subordinate_to_strategic_direction).
narrative_ontology:cs_axiom_status(allocation_subordinate_to_strategic_direction, holdable).
narrative_ontology:cs_axiom_grounding('5046eb57-8330-491e-9a74-e3cf29fc7eb3', allocation_subordinate_to_strategic_direction, instrumental).
narrative_ontology:cs_reference_frame('5046eb57-8330-491e-9a74-e3cf29fc7eb3', strategic_self_reliance_imperative).
narrative_ontology:cs_drift_state('5046eb57-8330-491e-9a74-e3cf29fc7eb3', contemporary_sanctions_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5046eb57-8330-491e-9a74-e3cf29fc7eb3', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, strategic_industrial_bureaucracy).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, household_savers).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_sector_firms).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, non_strategic_private_enterprises).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, chokepoint_leverage_thesis).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, self_reliance_catchup_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the strategic-industry lists, approves the multi-year plans, and ties cadre advancement to milestone delivery on self-sufficiency targets. Directs the state banking system's credit toward listed sectors and signs off on export-control and localization rules. Its standing with elites and public rests on delivering visible wins in chips, aerospace, and AI, and it can redefine which industries count as strategic at any time.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, central_planning_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Ministries and planning commissions that translate the center's directives into fund disbursement, project approval, and progress metrics. Budget shares, staffing, and promotion tracks all scale with the strategic mission's size, and senior officials' careers are built on it; pivoting to a different governing priority would strand that accumulated expertise and status.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, strategic_industrial_bureaucracy, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, strategic_industrial_bureaucracy, beneficiary).

% Firms and institutes in semiconductors, aerospace, quantum, and military-adjacent computing. They receive below-market credit, guaranteed procurement, protection from foreign competitors, and first call on talent. Deliverables are judged on strategic milestones rather than quarterly returns, and failure is cushioned by follow-on funding.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    powerful, generational, mobile, national).

% Large state-linked conglomerates that absorb the largest share of directed credit and subsidies. In exchange they accept loss-making strategic bets, employment mandates, party committees inside the firm, and direction of commercial decisions toward the self-sufficiency campaign. They profit from the arrangement and are also conscripted into carrying its burdens.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champions, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, national_champions, payer).

% Households whose bank deposits earn administratively capped returns while their savings pool finances loans to listed sectors. Capital controls limit moving wealth abroad; property and informal channels carry their own risks. Consumption's share of household budgets stays compressed as a result, with no formal channel to influence how the pool is allocated.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, household_savers, payer,
    moderate, biographical, trapped, national).

% Companies making food, apparel, appliances, and services for domestic consumers. They borrow at higher rates and shorter terms than listed strategic firms because the banking system steers credit by list membership, and they compete for labor and inputs against subsidized rivals. Staying viable means operating leaner than their access to capital would otherwise allow.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_sector_firms, payer,
    moderate, biographical, constrained, national).

% Private firms outside the strategic lists: no subsidy access, closer regulatory and political scrutiny, and expectations that business decisions align with the self-sufficiency campaign. Some shrink, some redirect toward supplying the champions, and a few relocate operations abroad where rules permit.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, non_strategic_private_enterprises, payer,
    moderate, biographical, constrained, national).

% Foreign equipment, IP, and component vendors targeted by export controls and localization mandates. They are progressively barred from the strategic market they helped build; their exclusion is precisely what the border rules exist to accomplish. They retain other global markets, which softens the blow but does not restore access.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, foreign_technology_suppliers, excluded,
    institutional, biographical, arbitrage, global).

% Economists, bankers, and entrepreneurs who argue capital should follow returns rather than lists. Within the system their position reads as second-guessing a security imperative, so publication is narrowed, careers stall, and many fall quiet; the argument survives mainly in academic journals and private briefings.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_allocation_advocates, excluded,
    moderate, biographical, constrained, national).

% Researchers inside and outside the country who track misallocation costs, productivity effects, and comparisons with other directed-investment episodes. They publish the numbers both sides cite and can see the whole flow-of-funds picture that no participant seat observes directly.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, development_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves real collective-action problems that markets under-supply: critical technologies with national-security spillovers (advanced semiconductors, aerospace, industrial software) are vulnerable to adversary chokepoints, private capital will not fund decades-horizon capability building at the required scale, and supply-chain localization needs economy-wide mobilization that only centralized direction can force on schedule.
% TRANSFER_FUNCTION: Moves capital, talent, and procurement demand from household savers (through administered deposit rates and capital controls), consumer-facing firms, and non-strategic private enterprises toward defense-adjacent technology sectors and national champions; moves market share from foreign technology suppliers to domestic substitutes behind export controls and localization mandates.
% ABSENT_VOICES: Market-allocation advocates are marginalized inside the system; households bear the financing burden with no formal voice in allocation decisions; foreign suppliers are deliberately kept out of the conversation their exclusion enforces. Consumer-welfare interests generally have no seat where the strategic lists are drawn.
% DISAPPEARANCE_RATIONALE: Credit would re-price toward returns, the consumer and services share of the economy would expand, strategic sectors would shrink to commercially viable cores, foreign suppliers would re-enter, and the regime's public justification would have to be rebuilt around some other delivered performance - the industrial structure and the political narrative resting on it would reorganize within years.
% FOUNDING_PROBLEM: Dependence on potential adversaries for chokepoint technologies - advanced chips, aircraft engines, design software, industrial equipment - hands those adversaries coercive leverage that can be exercised suddenly and catastrophically, as export-control episodes have shown; a late-developing great power judged it could not secure its position while core inputs sat under rival control.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the arrangement's beneficiaries: the sanctioning states' own export-control conduct attests that chokepoint leverage is real and deliberately wielded; the security-studies literature and the historical record of sanction episodes document the mechanism; economists hostile to directed investment nonetheless concede the vulnerability is genuine. What no outside source corroborates is the stronger operative claim that self-sufficiency is achievable at acceptable cost on the declared timeline - that remains asserted by the administrators alone.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.71: the transfer is large relative to the economy, decoupled from market signals by design, and sustained across the whole interval, but it is bounded by a real coordination function (resource_allocation Boltzmann floor 0.15) - some directed investment buys genuine security capability. Suppression 0.75: persistence requires actively policing capital, technology, and discourse boundaries - export controls, capital controls, list-based credit steering, and career consequences for open dissent - so the arrangement cannot persist passively. Theater 0.46: real capability is built (yields improve, substitution deepens in some segments) while announced self-sufficiency percentages, showcase fabs, and statistical milestones increasingly outrun engineering reality. Accessibility_collapse 0.45: market allocation survives outside the strategic lists, so alternatives are degraded and crowded rather than eliminated. Resistance 0.58: local governments game targets, firms chase subsidies over capability, savers seek informal exits, and economists press the misallocation critique. The temporal series run on one shared grid (seven points, t approximately year 2007+t) with all three metrics authored at every point; the trajectories are monotonic rather than cyclical - each sanction episode ratchets suppression and directed investment upward, and target inflation drives theater upward faster than capability delivery.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical facts. From the center and the line ministries the arrangement is sovereign necessity they personally administer: the same flow of funds that savers experience as repressed returns reads to them as mobilized national strength. National champions sit genuinely astride the divide - they collect the transfer and are conscripted into its burdens, so their computed position should land between pure beneficiaries and payers. Payer seats with trapped or constrained exit (households, list-excluded firms) should compute the most extractive experience; the mobile beneficiary seats the least. The engine derives this divergence from power, exit, and role declarations; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (defense_adjacent_tech_sectors, national_champions, strategic_industrial_bureaucracy) derive low directionality - the arrangement subsidizes them. Declared victims (household_savers, consumer_sector_firms, non_strategic_private_enterprises) derive high directionality, amplified for household_savers by trapped exit under capital controls and moderated for firms that retain partial mobility. Champions' dual position (secondary payer role) lifts their directionality above pure beneficiaries, reflecting the policy burdens they carry. The bureaucracy combines agenda-setting with benefit collection; its identity-locked exit reflects careers fused to the mission. Foreign_technology_suppliers are authored as excluded rather than victims: they are the enforcement object of the border rules, outside the domestic beneficiary/victim sets, and their arbitrage-grade exit (other global markets) keeps their effective burden below a trapped target's. National spatial scope keeps verification comparatively feasible, moderating the scope amplifier relative to a global-scope regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two opposite mislabels. Reading the arrangement as pure extraction ignores that its founding problem is corroborated live from outside the beneficiary set - chokepoint coercion is real, and some directed allocation is the price of addressing it; a snare label would erase the coordination function the security evidence supports. Reading it as pure coordination ignores the asymmetry: the transfer is decoupled from capability delivery, its costs fall on seats with no voice, and its beneficiaries are concentrated while its payers are diffuse. On mandatrophy: the founding problem is live, so no resolved-mandatrophy flag is declared; the drift risk runs toward theater accumulation rather than mandate death - if capability delivery stalls while milestone performance continues, the measurement series already records the rising theater ratio that would date such a transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the techno_nationalist_reading of the performance_legitimacy kernel; what structural features would change if a sibling reading (quantitative_growth_reading, qualitative_development_reading, livelihood_security_reading) were instantiated instead?',
    'Cross-reading comparison: re-author epsilon, beneficiaries, and victims under each sibling reading and diff the resulting structures. The disagreement is located in what counts as legitimate performance - security-capability milestones versus growth rates versus livelihood delivery - and each location implies a different victim set and transfer surface.',
    'Under quantitative_growth_reading the extraction profile shifts toward growth-composition distortion; under livelihood_security_reading the victim set expands to service users and patients while the strategic-sector beneficiary set shrinks; the computed classification could move between tangled_rope and snare depending on which reading''s allocation regime is measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one reading of the performance_legitimacy kernel among four; sibling readings are separate constraints, not positions inside this one.').

omega_variable(
    security_externality_genuineness,
    'Is the security coordination function genuine (chokepoint vulnerability justifying directed allocation) or substantially cover for rent collection by strategic sectors?',
    'Compare realized capability outcomes (yield rates, import-substitution depth, fielded systems) against cumulative subsidy volumes; test the chokepoint thesis against sanction-episode evidence; use the natural experiment of sectors with market exposure versus pure subsidy recipients.',
    'If predominantly cover, effective extraction exceeds the coordination floor by a wide margin and the constraint trends toward snare; if genuine, part of the measured extraction is the price of the security good itself and the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_externality_genuineness, empirical, 'Whether the security function justifying the coordination floor is real or a cover story.').

omega_variable(
    financial_repression_incidence,
    'What share of the strategic-investment transfer is financed by household savers through administered deposit rates and capital controls, versus fiscal revenue or retained enterprise earnings?',
    'Flow-of-funds analysis decomposing the sources of directed credit; comparison of household deposit returns against available shadow-market alternatives over the interval.',
    'Higher household incidence raises the payer seat''s directionality and amplifies effective extraction; a fiscally dominated mix lowers it and shifts victim weighting toward taxpayers and foregone public services.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_repression_incidence, empirical, 'Incidence of the financing burden across saver, taxpayer, and enterprise channels.').

omega_variable(
    capability_theater_boundary,
    'How much self-sufficiency activity produces deployable capability versus statistical and showcase performance (announced substitution rates, demonstration projects, milestone ceremonies)?',
    'Audit announced self-sufficiency percentages against customs/import data and disclosed yield figures; track utilization of showcase facilities after commissioning.',
    'A rising theater share dates a drift toward inertial maintenance even while gross investment grows; a low theater share supports the tangled_rope reading and the current theater_ratio trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_theater_boundary, empirical, 'Boundary between real capability delivery and performative milestone production.').

omega_variable(
    cs_framing_underdetermination,
    'Is the commitment system''s kernel the formalized plan documents (multi-year plans, strategic-industry lists), or the leadership''s legitimacy narrative that those documents instrument?',
    'Observe which layer absorbs drift: if plan targets are quietly revised while the narrative stays fixed, the narrative is the operative kernel; if documents bind practice, the formalized kernel governs.',
    'Framing the narrative as the operative kernel moves kernel_codification toward implicit and changes codification_collapse susceptibility; the current formalized framing predicts drift migrating into the interpretive layer rather than into the documents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of what the stabilized kernel is, with different classification consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(perf_tr_t0, observed).
narrative_ontology:measurement(perf_tr_t3, performance_legitimacy__techno_nationalist_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement_basis(perf_tr_t3, observed).
narrative_ontology:measurement(perf_tr_t6, performance_legitimacy__techno_nationalist_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(perf_tr_t6, observed).
narrative_ontology:measurement(perf_tr_t9, performance_legitimacy__techno_nationalist_reading, theater_ratio, 9, 0.36).
narrative_ontology:measurement_basis(perf_tr_t9, observed).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__techno_nationalist_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(perf_tr_t12, observed).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__techno_nationalist_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(perf_tr_t15, observed).
narrative_ontology:measurement(perf_tr_t18, performance_legitimacy__techno_nationalist_reading, theater_ratio, 18, 0.46).
narrative_ontology:measurement_basis(perf_tr_t18, observed).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(perf_be_t0, observed).
narrative_ontology:measurement(perf_be_t3, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(perf_be_t3, observed).
narrative_ontology:measurement(perf_be_t6, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(perf_be_t6, observed).
narrative_ontology:measurement(perf_be_t9, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement_basis(perf_be_t9, observed).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(perf_be_t12, observed).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement_basis(perf_be_t15, observed).
narrative_ontology:measurement(perf_be_t18, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 18, 0.71).
narrative_ontology:measurement_basis(perf_be_t18, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(perf_su_t0, observed).
narrative_ontology:measurement(perf_su_t3, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 3, 0.54).
narrative_ontology:measurement_basis(perf_su_t3, observed).
narrative_ontology:measurement(perf_su_t6, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement_basis(perf_su_t6, observed).
narrative_ontology:measurement(perf_su_t9, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 9, 0.65).
narrative_ontology:measurement_basis(perf_su_t9, observed).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(perf_su_t12, observed).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(perf_su_t15, observed).
narrative_ontology:measurement(perf_su_t18, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 18, 0.75).
narrative_ontology:measurement_basis(perf_su_t18, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'performance legitimacy' conflates four structurally distinct legitimacy arrangements (growth-rate delivery, high-quality transformation, livelihood delivery, strategic-technology self-sufficiency) with different epsilon values, beneficiary sets, and victim sets. Per the epsilon-invariance principle they are authored as separate constraints forming the performance_legitimacy family; this file is the techno-nationalist member. Edges run to all three siblings because this reading's allocation choices reshape each sibling's operating environment (crowding out livelihood fiscal space, distorting the growth mix, absorbing the innovation agenda). The livelihood edge is typed influences rather than coexists_with because credit and fiscal crowding-out structurally pressures that reading's deliverables, not merely competes with it in discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
