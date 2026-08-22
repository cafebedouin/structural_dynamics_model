% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Decarbonization Portfolio Legitimacy (Pragmatism Reading)
 *   domain: energy/climate/governance
 *
 * SUMMARY:
 *   This constraint encodes the portfolio pragmatism reading of climate
 *   mitigation legitimacy: the claim that decarbonization requires a
 *   technology-neutral portfolio including both nuclear and renewables, with
 *   regional and temporal variation in the optimal mix. This reading sits at
 *   the center of an ongoing kernel contest in climate and energy policy. The
 *   portfolio pragmatism frame is neither obviously false nor obviously
 *   true—it is a specific institutional choice about which technologies are
 *   treated as co-essential and how resource allocation should proceed. The
 *   constraint's operation involves legitimating particular pathways while
 *   suppressing others, and benefiting institutions positioned as arbiters of
 *   the technology mix (nuclear industry, utility operators, grid-expertise
 *   communities) while extracting from those advocating renewable-only or
 *   demand-reduction pathways. The author instantiates this reading as a
 *   Tangled Rope: it has a real coordination function (coordinating diverse
 *   pathways under uncertainty) AND asymmetric extraction (some parties are
 *   benefited, others are paying through the same structure), AND active
 *   enforcement (policy authorities, modeling frameworks, and institutional
 *   gatekeeping maintain the reading's legitimacy against competing
 *   alternatives).
 *
 * KEY AGENTS:
 *   - nuclear_industry_actors: Primary beneficiary (role: beneficiary + agenda_setter) — institutional power, arbitrage exit, collects sustained capital flows and policy legitimacy
 *   - integrated_utility_operators: Primary beneficiary (role: beneficiary) — institutional power, constrained exit, manages baseload and portfolio diversification
 *   - grid_stability_expertise_communities: Secondary beneficiary (role: beneficiary + agenda_setter) — institutional power, arbitrage exit, positioned as essential technology arbiters
 *   - renewable_energy_sector_investors: Primary victim (role: payer) — powerful, mobile exit, pay through dilution of urgency and capital allocation competition
 *   - climate_justice_constituencies: Secondary victim (role: payer) — powerless, trapped exit, bear temporal mismatch and distributional costs
 *   - climate_policy_authorities: Agenda-setter (role: agenda_setter) — institutional power, constrained exit, maintain the legitimacy frame and manage competing constituencies
 *   - renewable_primacy_advocates: Excluded (role: excluded) — organized power, constrained exit, sidelined from technology-choice legitimacy determination
 *   - analytical_observer_seat: Observes the constraint's operation (role: observer) — analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Decarbonization Portfolio Legitimacy (Pragmatism Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy/climate/governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '7092aa79-13af-4116-a172-b89418f2ca61').
narrative_ontology:cs_kernel_codification('7092aa79-13af-4116-a172-b89418f2ca61', fixed_text).
narrative_ontology:cs_authority_grounding('7092aa79-13af-4116-a172-b89418f2ca61', extraction).
narrative_ontology:cs_interpretation_layer_present('7092aa79-13af-4116-a172-b89418f2ca61').
narrative_ontology:cs_reading_relation('7092aa79-13af-4116-a172-b89418f2ca61', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7092aa79-13af-4116-a172-b89418f2ca61', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('7092aa79-13af-4116-a172-b89418f2ca61', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('7092aa79-13af-4116-a172-b89418f2ca61', foundational, neither_technology_privileged_a_priori).
narrative_ontology:cs_axiom_status(neither_technology_privileged_a_priori, holdable).
narrative_ontology:cs_axiom_grounding('7092aa79-13af-4116-a172-b89418f2ca61', neither_technology_privileged_a_priori, empirically_contingent).
narrative_ontology:cs_axiom('7092aa79-13af-4116-a172-b89418f2ca61', foundational, regional_variation_in_optimal_mix_necessary).
narrative_ontology:cs_axiom_status(regional_variation_in_optimal_mix_necessary, holdable).
narrative_ontology:cs_axiom_grounding('7092aa79-13af-4116-a172-b89418f2ca61', regional_variation_in_optimal_mix_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('7092aa79-13af-4116-a172-b89418f2ca61', uncertain_technology_trajectories_demand_growth_given).
narrative_ontology:cs_drift_state('7092aa79-13af-4116-a172-b89418f2ca61', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7092aa79-13af-4116-a172-b89418f2ca61', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_actors).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_utility_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, capital_equipment_vendors).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_stability_expertise_communities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_sector_investors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_justice_constituencies).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, decentralized_energy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear manufacturers, reactor operators, fuel-cycle companies, and waste-management contractors benefit from policy frameworks that treat nuclear as co-essential to decarbonization rather than optional or inferior. They set deployment strategy, participate in standard-setting for grid integration, and influence regulatory frameworks. A technology-neutral portfolio legitimacy keeps capital flowing and deployment timelines credible.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_actors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_actors, agenda_setter).

% Incumbent regional and national utilities operate generation portfolios mixing coal, gas, hydro, and increasingly renewables and nuclear. A portfolio pragmatism framework allows them to justify continued investment in capital-intensive baseload (particularly new nuclear) while renewables capacity expands, distributing risk across technologies and extending asset lifespans. They maintain operational control and earn returns on large infrastructure.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_utility_operators, beneficiary,
    institutional, generational, constrained, national).

% Renewable energy firms, wind and solar developers, battery manufacturers, and venture capital concentrated in distributed energy see portfolio pragmatism as a dilution vector: it keeps nuclear as a policy-equal alternative, competing for limited grid integration investment, grid capacity upgrades, and R&D funding. They argue renewables plus storage can meet targets faster at lower cost; the portfolio framework legitimates longer timelines and nuclear parity that reduces renewable deployment urgency.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_sector_investors, payer,
    powerful, biographical, mobile, global).

% Communities in the Global South, frontline populations bearing climate impacts now, and those sited near nuclear facilities or waste repositories argue the portfolio pragmatism frame obscures distributional harms: nuclear deployment perpetuates centralized, capital-intensive infrastructure that accrues benefits to wealthy regions and costs (waste, risk) to vulnerable ones. They are excluded from technology selection decisions and bear the temporal mismatch—climate urgency vs. nuclear construction timelines.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_justice_constituencies, payer,
    powerless, generational, trapped, global).

% Community solar developers, microgrids, local energy democracy movements, and small-scale renewable operators see the portfolio pragmatism frame as a structural barrier: it legitimates large, centralized generation (whether nuclear or utility-scale solar/wind) as the primary decarbonization path, suppressing policy and investment pathways toward distributed, locally-controlled renewable infrastructure and demand flexibility.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, decentralized_energy_advocates, payer,
    moderate, biographical, constrained, regional).

% System operators, grid engineers, and grid-stability research communities (national labs, universities) benefit from a portfolio framing that requires them as arbiters of the technology mix: their expertise in dispatch, stability, and integration becomes essential to answering 'what balance of nuclear and renewables does THIS grid need.' This expertise gate sustains their institutional role and research funding.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_stability_expertise_communities, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_stability_expertise_communities, agenda_setter).

% National climate ministries, regional energy authorities, and international climate frameworks (IPCC, UNFCCC) adopt and enforce the portfolio pragmatism reading in policy and assessment frameworks. They justify it as empirically grounded (mixed portfolios appear in decarbonization models) and politically feasible (does not eliminate any major energy sector). They manage the legitimacy claim and navigate the competing constituencies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_policy_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Renewable energy advocates, some climate NGOs, and decentralized-energy movements argue a renewable-plus-storage-only pathway is faster, cheaper, and more equitable. They are not excluded from all policy conversations but are systematically sidelined from technology-choice legitimacy frames: portfolio pragmatism does not privilege their analysis, and their proposed alternatives are treated as politically infeasible rather than technically sound.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_primacy_advocates, excluded,
    organized, biographical, constrained, global).

% Degrowth and post-growth advocates argue that rapid decarbonization without demand reduction is technologically implausible, and that portfolio pragmatism obscures this by implicitly assuming energy-demand growth will continue. They are excluded from legitimacy frames grounded in technology-neutral optimization; their premise (that demand reduction is NECESSARY, not optional) contradicts the assumption that supports the portfolio reading.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_theorists, excluded,
    moderate, civilizational, trapped, global).

% System analysts, academic researchers, and independent evaluators of decarbonization pathways observe the constraint's operation: the portfolio pragmatism frame shapes which models are credible, what timelines are deemed feasible, and which stakeholders are treated as legitimate voices in technology choice. The reading itself claims neutrality but constitutes a real institutional choice.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, analytical_observer_seat, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_actors).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decarbonization pathway research, grid integration planning, and capital allocation across multiple competing energy technologies by treating neither nuclear nor renewables as a priori privileged, allowing regional and temporal variation in the optimal mix. Solves the coordination problem of how much investment to direct to each technology given uncertainty about cost trajectories, grid integration constraints, and construction timelines.
% TRANSFER_FUNCTION: Moves research priority and capital deployment toward technologies and institutions (particularly large nuclear and utility-scale generation) that are positioned as co-necessary to any credible decarbonization scenario. Shifts investment flows away from potentially faster renewable+storage pathways and distributed energy models that would compete for the same capital and grid infrastructure.
% ABSENT_VOICES: Renewable primacy advocates and degrowth theorists are structurally excluded from legitimacy determination: the portfolio frame treats their technical claims (renewables+storage sufficiency, demand reduction necessity) as politically infeasible rather than engaging them as alternative technology hypotheses. Communities bearing uranium mining and nuclear waste impacts have no seat in the technology-choice conversation. Indigenous communities affected by dam construction for hydro or reservoir siting for nuclear have minimal voice in regional technology mix decisions.
% DISAPPEARANCE_RATIONALE: If the portfolio pragmatism legitimacy frame collapsed—replaced by renewable-only or demand-reduction-first frameworks—capital allocation to nuclear would shrink sharply, grid planning assumptions would shift, and research priorities would reorganize around faster renewable deployment and storage scaling. The entire institutional apparatus (utility investment strategies, policy modeling, regulatory timelines, expertise institutions) is organized around the portfolio assumption. Disappearance would trigger rapid reallocation of billions in annual infrastructure investment and force renegotiation of energy independence and supply-chain strategies.
% FOUNDING_PROBLEM: Decarbonization modeling showed multiple pathways to climate targets; no single technology was sufficient at needed scale, speed, and cost in 1995-2005 conditions. How should policymakers allocate resources across competing technologies without privileging unproven solutions or locking in inferior paths?
% FOUNDING_PROBLEM_CORROBORATION: Integrated Assessment Models (IEMs) used by the IPCC show diversified technology portfolios in many decarbonization pathways, which utilities and policy authorities cite as support for the portfolio reading. However, renewable-sector researchers and independent analysts counter that IEM portfolios are artifacts of model parameterization and cost assumptions that embed status-quo bias toward incumbent technologies; they attest the technical case for renewable-plus-storage has strengthened faster than the models assumed. No consensus external authority endorses the portfolio reading as the unique optimal framing—it is one interpretation among contested others. The European Commission's 2021 renewable directive and Denmark's 80% renewable electricity achievement (2024) suggest renewable-only pathways are technically feasible; however, China's continued nuclear expansion and US policy maintaining nuclear-renewable parity suggest the portfolio reading remains institutionally embedded in major economies.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 (1995) to 0.42 (2024) as the portfolio framing becomes institutionalized: in 1995, portfolio approaches were one option among many; by 2024, they are embedded in IPCC modeling, national climate policy, and utility strategy—a form of extraction via legitimacy capture. The reading's core claim (technology-neutral mix is necessary) is neither obviously false nor empirically settled; its institutional embedding despite ongoing contestation is what constitutes extraction. Suppression rises from 0.15 to 0.38 over the same period as policy authorities, standard-setting bodies, and modeling communities actively enforce the reading against competing framings—renewable-only and demand-reduction pathways are treated as politically infeasible rather than as competing technical hypotheses. Theater ratio (0.28 in 2024) reflects that part of the enforcement activity defends 'neutrality' as cover for what is actually a specific technology choice. Accessibility collapse (0.62) is moderate because renewable advocates retain some access to policy conversations but are systematically sidelined from legitimacy determination. Resistance (0.71) is substantial: climate NGOs, renewable sector, and degrowth movements actively contest the portfolio framing, and the contest has not been resolved despite two decades of institutional embedding.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and the victim seats should compute to different types. From the nuclear industry and utility operator positions, the constraint appears as genuine coordination—a pragmatic response to heterogeneous uncertainties (cost trajectories, grid integration challenges, construction timelines) that allows multiple pathways and regional variation. From the renewable sector and climate justice seats, the same constraint appears as extraction—a legitimacy frame that privileges capital-intensive, centralized technologies and suppresses faster, more equitable alternatives. The policy authority seat experiences the constraint as enforcement of a balance between competing factions, but that enforcement activity itself constitutes suppression of the alternatives. The engine computes these per-seat divergences from the structural data: beneficiary seats with arbitrage exit and institutional power will classify differently from powerless victim seats with trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry actors are full beneficiaries (d near 0.0): they collect sustained capital flows, policy legitimacy, and deployment certainty from the portfolio frame being institutionalized. Integrated utilities are beneficiaries (d ~0.1-0.2): they benefit from portfolio flexibility and sustained baseload investment but face some constraint from renewable growth pressures. Renewable sector investors are targets (d ~0.75-0.85): they pay through diluted urgency, slower deployment timelines, and competition for limited grid upgrade capital, despite their technical advantage. Climate justice constituencies are targets (d ~0.9): they bear temporal mismatch (climate urgency vs. nuclear construction timelines) and distributional harm (waste and siting risks), with no exit. Grid stability expertise communities are beneficiaries (d ~0.05-0.15): they are positioned as essential arbiters, sustaining their institutional role and research funding. Policy authorities sit near symmetric (d ~0.5): they benefit from the appearance of neutrality and political viability but pay through the complexity of managing competing constituencies and the risk of being delegitimized if the reading fails. Renewable advocates are partially excluded and partially suppressed targets (d ~0.8): the reading does not explicitly harm them but systematically sidelines their claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (How should policymakers allocate resources across competing technologies without privileging unproven solutions?) was live in 1995 when multiple pathways were genuinely uncertain. By 2024, the problem is contested: renewable cost curves have fallen faster than models assumed, storage is scaling faster than predicted, and the technical case for renewable-only pathways has strengthened substantially. However, the portfolio pragmatism institutional apparatus treats the founding problem as still live, justifying continued nuclear investment and parity with renewables. This is a candidate for mandatrophy: the founding problem's status has shifted from live to dead or at least substantially resolved, but the arrangement persists and has become extractive. The constraint manifests as theater—'neutrality' language defending a specific technology choice against competing alternatives. If the founding problem is dead (renewable+storage sufficiency is now empirically plausible at needed scale), the constraint's legitimacy is compromised, and its persistence depends on institutional inertia and beneficiary power rather than on the coordination it claims to solve. Omega variables address this contest directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_status_drift,
    'Has the founding problem (how to allocate resources across uncertain technologies) actually been resolved by subsequent learning, or does it remain legitimately live?',
    'Comparative analysis of integrated assessment model cost assumptions and grid-integration constraints against empirical renewable and storage scaling curves (2024 forward). If renewable+storage performance tracks or exceeds model optimism while nuclear costs remain at or above model baseline, the founding problem is dead; if nuclear remains cost-competitive or superior, it is live.',
    'If the founding problem is dead (renewal+storage sufficiency demonstrated), the constraint transitions from Tangled Rope (coordination + extraction) to Snare (pure extraction defended by legitimacy cover). If it remains live, the Tangled Rope classification holds. This is the critical mandatrophy vector.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_status_drift, empirical, 'Whether rapid learning has resolved the technology-choice uncertainty the portfolio framing was built to address.').

omega_variable(
    technology_neutrality_performance,
    'Is the portfolio pragmatism frame genuinely technology-neutral (treats nuclear and renewables with symmetrical opportunity and burden), or does it embed asymmetric assumptions favoring one technology?',
    'Audit of integrated assessment models for parameterization bias: do nuclear cost assumptions reflect learning curves and scaling benefits symmetrically with renewable curves? Do grid-integration constraints assume equal flexibility from dispatchable nuclear and flexible renewable+storage mixes? Independent economic modeling with alternative cost and performance assumptions.',
    'If the frame is genuinely neutral, the claimed coordination function is real and the constraint is closer to Rope. If neutrality is rhetorical cover for nuclear advantage, the constraint is Snare. The grounding of the reading''s legitimacy depends on whether ''pragmatism'' means empirically correct optimization or ideologically acceptable compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_performance, empirical, 'Whether the portfolio framing''s claimed neutrality is matched by actual symmetric treatment of technologies in policy and modeling.').

omega_variable(
    kernel_contest_framing,
    'Is the climate mitigation legitimacy kernel a genuine technical question (What technologies ARE necessary?) or a political question (Which technology choices will be institutionally acceptable?) framed as technical?',
    'Genealogical analysis: tracing when and how the portfolio pragmatism reading was adopted by policy authorities versus when the technical sufficiency of renewable+storage was demonstrated. If adoption preceded technical consensus, framing is political-as-technical.',
    'If the kernel is genuinely technical, all readings are competing hypotheses and the engine''s role is measuring which one best survives empirical test. If it is political framing disguised as technical, all readings are competing legitimacy claims and the constraint is primarily about power. This affects interpretation of the entire constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'The ontological status of the contested kernel: is it a scientific question or a political choice framed scientifically?').

omega_variable(
    demand_reduction_assumption_implicit,
    'Does the portfolio pragmatism reading depend on an implicit assumption that energy demand will grow (making large-scale generation expansion necessary), such that it forecloses degrowth_sufficiency_reading by premise rather than argument?',
    'Analysis of integrated assessment models and policy documents using portfolio pragmatism: do they model demand-reduction scenarios alongside technology-mix scenarios, or does demand growth appear as a constant? If demand growth is assumed exogenous, degrowth framing is not genuinely contested—it is excluded by model boundary.',
    'If demand growth is implicit and exogenous, the portfolio pragmatism reading forecloses degrowth reading by premise, not by logic—they cannot coexist in the same framework because one assumes demand as a variable to be optimized over and the other assumes it as a parameter to be questioned. This would elevate their relationship from coexists_with to forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demand_reduction_assumption_implicit, conceptual, 'Whether portfolio pragmatism depends on demand-growth assumption that forecloses degrowth framing by assumption rather than argument.').

omega_variable(
    regional_variation_genuine_or_rhetorical,
    'Does the portfolio pragmatism frame genuinely permit regional variation in optimal technology mix (its claimed structural feature), or is the variation rhetorical while actual policy pushes uniform nuclear-renewable parity across all regions?',
    'Policy analysis: do climate authorities permit regions with superior renewable resources or renewable-friendly politics to pursue renewable-only pathways? Or do they insist on nuclear inclusion in every regional decarbonization plan?',
    'If regional variation is genuine, the reading has flexibility and lower extraction. If uniform parity is enforced despite regional differences, the reading is extractive across all seats—it uses ''regional variation'' rhetoric to legitimize universal nuclear deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variation_genuine_or_rhetorical, empirical, 'Whether the reading''s claimed regional variation permission is honored in practice or suppressed by uniform nuclear-parity requirement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 1995, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1995, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(clim_tr_t2005, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(clim_tr_t2012, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(clim_tr_t2018, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(clim_tr_t2024, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2030, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t1995, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(clim_be_t2005, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(clim_be_t2012, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(clim_be_t2018, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(clim_be_t2024, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2030, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1995, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(clim_su_t2005, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2005, 0.24).
narrative_ontology:measurement(clim_su_t2012, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2012, 0.32).
narrative_ontology:measurement(clim_su_t2018, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2018, 0.36).
narrative_ontology:measurement(clim_su_t2024, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2024, 0.38).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2030, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel. The kernel contest involves four incompatible legitimacy claims about what decarbonization requires: (1) portfolio pragmatism (this reading): neither nuclear nor renewables privileged a priori, regional variation in mix; (2) baseload necessity: renewables cannot provide dispatchable baseload at scale; (3) renewable primacy: renewables+storage can achieve full decarbonization faster and cheaper; (4) degrowth sufficiency: demand reduction makes large-scale generation expansion unnecessary. Each reading instantiates a different ε, different beneficiary/victim structure, and different type. They are not different measurements of the same constraint—they are different constraints, each a reading of the contested kernel. All members of the family must link via network.affects_constraints to enable contamination propagation analysis and constraint family tracing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
