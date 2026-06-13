% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Climate Technology Velocity Legitimacy Gate (2030/2050 Carbon Budget)
 *   domain: energy/climate/technology_governance
 *
 * SUMMARY:
 *   Under the velocity-primacy reading of the technology legitimacy kernel, a
 *   climate mitigation technology is deemed legitimate if and only if it can
 *   be deployed at scale within the 2030/2050 carbon budget timeline. This
 *   reading frames near-term deployment speed as THE decisive criterion for
 *   justifying technology investment and policy support. Solar and wind
 *   technologies, which scale from design to grid operation in 2–5 years, are
 *   beneficiaries; nuclear, carbon capture, and long-lead baseload
 *   technologies are structurally disadvantaged because their 10–20 year
 *   construction timelines place them outside the legitimacy window. Grid
 *   operators, who must manage increasing intermittency from rapid renewable
 *   scaling, bear operational costs and bear the extraction. The kernel
 *   itself is contested: a precautionary reading prioritizes bounded failure
 *   modes and reversibility; a reliability-primacy reading prioritizes
 *   dispatchable baseload capacity. This story instantiates the velocity
 *   reading in isolation, mapping its structural entailments without
 *   resolving the kernel contest.
 *
 * KEY AGENTS:
 *   - renewable_energy_developers (beneficiary, powerful, mobile) — gain capital access and policy legitimacy from velocity criterion
 *   - grid_operators (payer, organized, constrained) — absorb intermittency management burden without control over deployment timelines
 *   - nuclear_technology_programs (payer/excluded, institutional, identity_locked) — excluded by velocity gate despite low-carbon attributes
 *   - baseload_dependent_jurisdictions (payer, moderate, constrained) — must invest in backup infrastructure or accept grid stress
 *   - climate_finance_allocators (agenda_setter, institutional) — enforce velocity gate through budget and policy criteria
 *   - energy_stability_analysts (observer, analytical) — provide technical evidence on grid stability feasibility
 *   - precautionary_framework_advocates (excluded) — removed from legitimacy conversation despite expertise in technology assessment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.67).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.58).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Climate Technology Velocity Legitimacy Gate (2030/2050 Carbon Budget)").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy/climate/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '1e43ce9d-4f60-4628-beec-4fce50ae41c5').
narrative_ontology:cs_kernel_codification('1e43ce9d-4f60-4628-beec-4fce50ae41c5', formalized).
narrative_ontology:cs_authority_grounding('1e43ce9d-4f60-4628-beec-4fce50ae41c5', extraction).
narrative_ontology:cs_interpretation_layer_present('1e43ce9d-4f60-4628-beec-4fce50ae41c5').
narrative_ontology:cs_reading_relation('1e43ce9d-4f60-4628-beec-4fce50ae41c5', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e43ce9d-4f60-4628-beec-4fce50ae41c5', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('1e43ce9d-4f60-4628-beec-4fce50ae41c5', foundational, near_term_velocity_primacy).
narrative_ontology:cs_axiom_status(near_term_velocity_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1e43ce9d-4f60-4628-beec-4fce50ae41c5', near_term_velocity_primacy, empirically_contingent).
narrative_ontology:cs_axiom('1e43ce9d-4f60-4628-beec-4fce50ae41c5', foundational, carbon_budget_temporal_scarcity).
narrative_ontology:cs_axiom_status(carbon_budget_temporal_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('1e43ce9d-4f60-4628-beec-4fce50ae41c5', carbon_budget_temporal_scarcity, empirically_contingent).
narrative_ontology:cs_reference_frame('1e43ce9d-4f60-4628-beec-4fce50ae41c5', velocity_as_primary_selection_criterion).
narrative_ontology:cs_drift_state('1e43ce9d-4f60-4628-beec-4fce50ae41c5', contemporary_grid_stress_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e43ce9d-4f60-4628-beec-4fce50ae41c5', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, distributed_generation_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, baseload_dependent_jurisdictions).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_technology_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, stranded_asset_holders).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, stranded_asset_holders).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_scarcity).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, near_term_decarbonization_urgency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar and wind technology developers gain legitimacy and capital access under velocity criterion because their technologies scale from design to grid operation in 2–5 years. They benefit from accelerated permitting, concessional climate finance, and policy support justified by speed-to-mitigation argument. They can relocate deployments to jurisdictions with favorable velocity framings if one jurisdiction tightens criteria.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers, beneficiary,
    powerful, biographical, mobile, global).

% Manage electric grids transitioning to high renewable penetration. The velocity gate privileges deployment speed over grid stability planning, forcing investment in backup capacity, storage, demand response, and grid hardening on timescales not aligned with infrastructure planning cycles. They cannot relocate (infrastructure is fixed) and cannot negotiate with upstream allocators who set the gate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    organized, generational, constrained, national).

% Large-scale nuclear deployment programs (advanced reactors, SMRs) are structurally disadvantaged because 10–20 year construction timelines place them outside velocity window. Programs are identity-locked into nuclear models and cannot pivot to renewables without institutional dissolution. Velocity gate offers no path to legitimacy despite zero-carbon attributes; programs face defunding or acceptance of illegitimacy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_technology_programs, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_technology_programs, excluded).

% Regions with industrial loads or dispatchability requirements face cost escalation from velocity-optimized renewable scaling. Must invest in rapid-deployment backup infrastructure (gas peaker plants, battery storage) or accept grid stress. Cannot diversify generation sources quickly; energy infrastructure is capital-intensive and localized.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, baseload_dependent_jurisdictions, payer,
    moderate, generational, constrained, regional).

% International climate funds, development banks, national energy agencies enforce velocity gate through funding criteria, technology prioritization, and policy guidance. They define which technologies receive accelerated permitting and concessional financing. Institutional power allows them to set the gate; analytical exit allows policy revision, but institutional structures slow such revision.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_finance_allocators, agenda_setter,
    institutional, generational, analytical, global).

% Utilities and energy companies holding coal, gas, nuclear infrastructure face accelerated write-downs under velocity gate. Some benefit if they pivot quickly to renewables; others with locked-in fuel contracts face constrained exit as capital is stranded. Directionality is mixed: powerful in lobbying, constrained in operational flexibility.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, stranded_asset_holders, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, stranded_asset_holders, beneficiary).

% Grid researchers, reliability engineers, energy analysts study whether velocity-maximized deployment maintains grid stability. They measure constraint outputs: Can 2030/2050 targets and grid reliability be jointly achieved with velocity-optimized technology sets? Analytical position; feed technical evidence into kernel contest.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, energy_stability_analysts, observer,
    analytical, biographical, analytical, global).

% Proponents of technology assessment and reversibility criteria (intergenerational justice scholars, waste stewardship experts) are absent from velocity gate's framing. Their arguments about legacy costs and intergenerational burden are not part of legitimacy test under this reading. Excluded not by force but by construction: velocity is sole criterion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_framework_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns technology development, capital investment, and policy urgency around velocity-to-deployment as the primary criterion for climate mitigation legitimacy. Solves collective-action problem where diverse actors would pursue conflicting technology roadmaps (renewables vs. nuclear vs. CCS) absent unified speed-based criterion, fragmenting investment and prolonging transition.
% TRANSFER_FUNCTION: Moves legitimacy, public capital, fast-track permitting, and R&D funding FROM slow-deployment technologies (nuclear, CCS, advanced gas) TO fast-deployment renewables and storage. Also transfers operational burden (grid intermittency management cost) FROM technology developers TO grid operators managing integration at scale.
% ABSENT_VOICES: Nuclear engineering communities; grid stability/reliability experts; workers dependent on conventional generation; long-term waste stewardship practitioners; engineers focused on technology reversibility and intergenerational cost; precautionary assessment researchers.
% DISAPPEARANCE_RATIONALE: If velocity gate vanished, technology legitimacy criteria would revert to competing frameworks (reliability, precaution, cost-effectiveness). Capital would redirect toward baseload alternatives; policy timelines would decouple from 2030/2050; nuclear programs would resume investment; grid planning horizons would extend. Energy technology landscape would reorganize around different criteria within months.
% FOUNDING_PROBLEM: Atmospheric carbon budget is finite and decarbonization timelines short (net-zero 2050, steep reductions 2030). If technology deployment cycles are 10–20 years, later-stage technologies will come online after carbon budget is exhausted. Founding problem: select technologies deployable within remaining timeline to reduce absolute emissions before budget closure.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group III empirical assessments confirm carbon budget constraints and 2030/2050 deployment timelines are binding. Independent analyses (MIT, Brookings, energy research institutes outside renewable advocacy) corroborate carbon-budget scarcity as real constraint. The founding problem is contested on whether it requires velocity-as-sole-criterion, but the underlying scarcity is widely corroborated outside beneficiary advocacy set. Nuclear engineers and reliability researchers dispute whether the scarcity justifies excluding dispatchable alternatives.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.67 at interval end) reflects that the velocity gate privileges one technology class over another through rules that are decoupled from whether the excluded technologies could deliver equal or superior mitigation under different constraints. Suppression (0.58) captures active enforcement: nuclear programs face accelerated defunding, baseload capacity investment slows, grid flexibility planning is constrained by velocity-optimized deployment curves. Theater ratio rises from 0.22 to 0.42 over the interval, indicating that as enforcement matures, an increasing share of resource allocation effort is spent justifying velocity primacy against rival framings (precautionary, reliability) rather than solving the underlying coordination problem (carbon budget scarcity). Accessibility of alternatives collapses asymmetrically by level: at individual level (e.g., a utility deciding plant investment), alternatives close off completely (0.75 at t25); at structural level (global energy governance), alternative framings remain institutionalized (0.63 at t25), marking the ongoing kernel contest. Resistance is high (0.72 baseline) because grid operators, nuclear engineers, and reliability researchers mount sustained objections; resistance declines slightly over time as beneficiaries entrench politically and suppress countervailing evidence. The measurements track the interval (0 to 25 years from circa 2000 to 2025), showing extractiveness and suppression rising monotonically as velocity becomes the governing criterion in real policy, while theater rises as legitimacy claims become increasingly performative (e.g., 'we are accelerating deployment' becomes more rhetorical, less empirical, as bottlenecks emerge).
 *
 * PERSPECTIVAL GAP:
 *   Velocity-primacy seat (climate allocators): 'We are forced to choose: deploy the fastest mitigation now or miss the carbon budget entirely. Excluding slow technologies is rational triage, not extraction.' Payer seat (grid operators): 'The velocity gate is dumping grid stability costs onto us without compensation. We did not choose this transition timeline.' Excluded seat (nuclear engineers): 'The gate is cherry-picking one dimension of technology suitability and calling it the whole story. Our technology could run baseload in 2040; the gate bars us from even being considered.'
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers benefit from the velocity gate: their technologies align with the criterion and gain accelerated capital and policy support. Directionality d ≈ 0.2 (near beneficiary end): they collect gains without bearing the constraint's operational costs; they have mobile exit options (can deploy in any jurisdiction adopting velocity primacy). Grid operators are targets: the constraint imposes intermittency management burden without compensating them or giving them control over deployment speed. Directionality d ≈ 0.75–0.80: they cannot exit (infrastructure is fixed), cannot negotiate the terms (the gate is set by allocators upstream), and accumulate costs continuously as renewable scaling accelerates. Nuclear programs are identity-locked into technologies that the gate excludes. Directionality d ≈ 0.85–0.90: they are targets of exclusion; they cannot pivot to competing technologies without dissolving their institutional identity; their exit from the space is forced, not chosen. Baseload jurisdictions sit at d ≈ 0.70: they bear cost escalation (backup infrastructure investment) imposed by the velocity gate's bias against their native generation mix. Climate finance allocators have near-zero directionality toward the constraint: they set and benefit from it; d ≈ 0.1. No directionality overrides are needed: the structural derivation (beneficiary + mobile exit → low d; victim + trapped/identity_locked exit → high d) is accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The velocity-primacy reading avoids the mandatrophy of confusing 'real coordination' with 'whose criteria dominate the discourse.' The founding coordination problem is genuine: carbon budgets are finite and time-limited, and selecting technologies that can reduce absolute emissions in the near term solves a real collective-action problem that would otherwise fragment technology investment. The asymmetry is real and structural: the gate privileges one technology class and imposes costs on operators and excluded alternatives, justified by reference to the carbon budget but decoupled from questions of whether excluded technologies could eventually deliver superior outcomes or whether grid stability is a legitimate constraint. The tangled rope classification holds: the constraint both solves a real coordination problem (alignment on near-term carbon reduction) AND asymmetrically extracts from grid operators and nuclear programs. It is not a snare because the coordination function is genuine and the beneficiaries are not collecting pure rents—renewables are deploying and reducing emissions. It is not a rope because the extraction is real: payers bear costs they did not impose and have no mechanism to influence the decision. The mandatrophy that would arise is if the reading tried to claim the gate is ONLY coordination (rope) or ONLY extraction (snare), when it is structurally both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grid_stability_feasibility,
    'Can grid stability and reliability be maintained at the deployment velocity prioritized by the velocity-primacy reading, or will intermittency and variability challenges force reversion to reliability-constrained technology selection?',
    'Real-time grid data from jurisdictions that have pursued velocity-maximized renewable deployment (California, Denmark, Texas ERCOT). Measure: Do blackout frequency, demand response activation, or grid stress indicators exceed pre-transition baselines? If yes, the velocity reading is generating a sustainability constraint that forces concessions to reliability criteria.',
    'If grid stability cannot be maintained under velocity-prioritized deployment, the reliability-primacy reading gains structural force (forced to slow renewable deployment or invest in baseload backup, both contrary to velocity primacy). The velocity reading would be revealed as unstable: the coordination it achieves on carbon budget alignment undermines another coordination goal (grid stability). Classification would shift from tangled rope toward snare for grid operators (imposed costs without compensation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_stability_feasibility, empirical, 'Whether velocity-optimized deployment is sustainable for grid operation or generates hidden constraints.').

omega_variable(
    technology_reversibility_lock_in,
    'Does choosing velocity-legitimized renewables lock in infrastructure, financing arrangements, and political commitments that foreclose later correction to include reliability or precautionary criteria?',
    'Historical analysis of technology transitions: once renewable infrastructure and financing models (PPAs, grid integration contracts, storage architecture) are built around velocity assumptions, can policy later incorporate baseload or precautionary constraints without stranded assets and unrecoverable investment? Comparative case: nuclear phase-out in Germany vs. recent policy reconsideration in EU policy.',
    'If velocity-optimized deployment creates lock-in that forecloses later alternatives, the precautionary reading gains force: early velocity choices become intergenerational constraints that later cohorts cannot easily undo. This would suggest the velocity reading should itself carry precautionary bounds (sunset clauses, reversibility requirements) to avoid converting a transitional gate into an inescapable trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_reversibility_lock_in, empirical, 'Whether the velocity reading''s decisions are reversible or lock in trajectory.').

omega_variable(
    kernel_reading_foreclosure,
    'Does adopting velocity-primacy as the governing legitimacy criterion foreclose the reliability-primacy reading, or can both readings coexist as different constraints on the same technology set?',
    'Institutional mapping: Which jurisdictions/authorities adopt velocity as sole criterion vs. which ones use velocity AND reliability as conjoint criteria? If all velocity-adopters eventually face grid stress and shift toward reliability constraints, the readings do not coexist stably. If some jurisdictions maintain velocity without stability degradation, the readings are coexistent (different parties can adopt different readings). If velocity is systematically seen as overriding reliability, foreclosure obtains.',
    'If the readings foreclose each other, the kernel contest is eventually zero-sum and resolved by institutional power (whoever controls policy allocation). If the readings coexist, they remain live alternatives held by different coalitions, and the velocity reading is one live policy option among others, not a universal governing principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the velocity reading logically excludes the reliability reading or permits coexistence.').

omega_variable(
    carbon_budget_scarcity_vindication,
    'Does the founding problem (carbon budget scarcity forcing velocity choice) remain empirically live, or has the carbon budget constraint been empirically superseded by technological progress, carbon removal capacity, or policy shift?',
    'IPCC and energy modeling community assessments: Do updated carbon budget estimates, including negative-emissions technology potential and policy acceleration, still require velocity-prioritized deployment to meet 2050 net-zero? If carbon budgets expand (e.g., breakthrough in carbon removal) or timelines shift (e.g., 2060 rather than 2050), the founding problem''s status changes from ''live'' to ''contested'' or ''dead'', potentially shifting the constraint from tangled rope (justified by real scarcity) to snare (justified by obsolete scarcity).',
    'If the founding problem dies (carbon budget is no longer the binding constraint), the velocity gate becomes mandatrophic: it persists through institutional inertia and beneficiary entrenchment, not through the coordination problem it was built to solve. The reading would shift from justified asymmetric extraction to pure capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_budget_scarcity_vindication, empirical, 'Whether the carbon budget scarcity that justifies velocity-primacy remains empirically binding.').

omega_variable(
    precautionary_irreversibility_trade_off,
    'Does the velocity reading''s bias toward fast deployment systematically exclude technologies with low long-term reversibility costs (e.g., nuclear waste) in favor of technologies with potentially higher long-term environmental or social costs but better velocity profiles?',
    'Life-cycle assessment and intergenerational cost accounting: Compare long-term liabilities of velocity-optimized renewable deployment (rare-earth mining, panel disposal, storage degradation, land use conflicts) against excluded technologies (nuclear waste stewardship). If velocity-optimized deployment creates larger intergenerational burdens than precautionary-included alternatives, the precautionary reading is vindicated as a corrective to velocity monoculture.',
    'If velocity-optimized deployment is revealed to impose higher intergenerational costs, the velocity reading becomes mandatrophic in a second-order sense: it solves a near-term coordination problem by imposing a long-term reversibility burden that violates principles of intergenerational justice. The reading would require a precautionary constraint (reversibility bounds) to be legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precautionary_irreversibility_trade_off, empirical, 'Whether velocity optimization trades near-term mitigation for long-term intergenerational burden.').

omega_variable(
    benignity_gate_application,
    'Does the velocity-primacy reading apply its legitimacy gate uniformly across technology types and geographies, or do beneficiaries (renewable developers) receive exemptions from velocity criteria (e.g., longer timelines for grid integration, storage maturation) while payers (nuclear, baseload) face absolute velocity enforcement?',
    'Policy document analysis: Are velocity criteria applied as hard gates or soft targets? Do renewable projects receive timeline extensions and learning allowances while nuclear projects face accelerated licensing and fixed-date deadlines? Asymmetric application would indicate the gate functions as benignity capture—the beneficiary set gets latitude while the payer set gets enforcement.',
    'If the velocity gate is applied asymmetrically (soft on renewables, hard on nuclear), it is revealed as a capture mechanism, not a neutral coordination rule. This would lower the constraint from tangled rope (justified asymmetry + coordination) toward snare (unjustified asymmetry masquerading as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(benignity_gate_application, empirical, 'Whether the velocity gate is enforced uniformly or asymmetrically to advantage beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(tech_tr_t5, observed).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(tech_tr_t10, observed).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(tech_tr_t15, observed).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(tech_tr_t20, projected).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(tech_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(tech_be_t5, observed).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(tech_be_t10, observed).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(tech_be_t15, observed).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(tech_be_t20, projected).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(tech_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(tech_su_t5, observed).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(tech_su_t10, observed).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement_basis(tech_su_t15, observed).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(tech_su_t20, projected).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(tech_su_t25, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(tech_grid_01, technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(tech_grid_02, technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse(class), 25, 0.72).
narrative_ontology:measurement(tech_grid_03, technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(tech_grid_04, technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse(individual), 25, 0.75).
narrative_ontology:measurement(tech_grid_05, technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(tech_grid_06, technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse(organizational), 25, 0.68).
narrative_ontology:measurement(tech_grid_07, technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(tech_grid_08, technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse(structural), 25, 0.63).
narrative_ontology:measurement(tech_grid_09, technology_legitimacy_kernel__velocity_primacy_reading, resistance(class), 0, 0.78).
narrative_ontology:measurement(tech_grid_10, technology_legitimacy_kernel__velocity_primacy_reading, resistance(class), 25, 0.71).
narrative_ontology:measurement(tech_grid_11, technology_legitimacy_kernel__velocity_primacy_reading, resistance(individual), 0, 0.72).
narrative_ontology:measurement(tech_grid_12, technology_legitimacy_kernel__velocity_primacy_reading, resistance(individual), 25, 0.65).
narrative_ontology:measurement(tech_grid_13, technology_legitimacy_kernel__velocity_primacy_reading, resistance(organizational), 0, 0.74).
narrative_ontology:measurement(tech_grid_14, technology_legitimacy_kernel__velocity_primacy_reading, resistance(organizational), 25, 0.68).
narrative_ontology:measurement(tech_grid_15, technology_legitimacy_kernel__velocity_primacy_reading, resistance(structural), 0, 0.68).
narrative_ontology:measurement(tech_grid_16, technology_legitimacy_kernel__velocity_primacy_reading, resistance(structural), 25, 0.62).
narrative_ontology:measurement(tech_grid_17, technology_legitimacy_kernel__velocity_primacy_reading, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(tech_grid_18, technology_legitimacy_kernel__velocity_primacy_reading, stakes_inflation(class), 25, 0.76).
narrative_ontology:measurement(tech_grid_19, technology_legitimacy_kernel__velocity_primacy_reading, stakes_inflation(individual), 0, 0.68).
narrative_ontology:measurement(tech_grid_20, technology_legitimacy_kernel__velocity_primacy_reading, stakes_inflation(individual), 25, 0.81).
narrative_ontology:measurement(tech_grid_21, technology_legitimacy_kernel__velocity_primacy_reading, stakes_inflation(organizational), 0, 0.54).
narrative_ontology:measurement(tech_grid_22, technology_legitimacy_kernel__velocity_primacy_reading, stakes_inflation(organizational), 25, 0.68).
narrative_ontology:measurement(tech_grid_23, technology_legitimacy_kernel__velocity_primacy_reading, stakes_inflation(structural), 0, 0.41).
narrative_ontology:measurement(tech_grid_24, technology_legitimacy_kernel__velocity_primacy_reading, stakes_inflation(structural), 25, 0.55).
narrative_ontology:measurement(tech_grid_25, technology_legitimacy_kernel__velocity_primacy_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(tech_grid_26, technology_legitimacy_kernel__velocity_primacy_reading, suppression(class), 25, 0.62).
narrative_ontology:measurement(tech_grid_27, technology_legitimacy_kernel__velocity_primacy_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(tech_grid_28, technology_legitimacy_kernel__velocity_primacy_reading, suppression(individual), 25, 0.65).
narrative_ontology:measurement(tech_grid_29, technology_legitimacy_kernel__velocity_primacy_reading, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(tech_grid_30, technology_legitimacy_kernel__velocity_primacy_reading, suppression(organizational), 25, 0.58).
narrative_ontology:measurement(tech_grid_31, technology_legitimacy_kernel__velocity_primacy_reading, suppression(structural), 0, 0.36).
narrative_ontology:measurement(tech_grid_32, technology_legitimacy_kernel__velocity_primacy_reading, suppression(structural), 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__velocity_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% The technology_legitimacy_kernel hosts three structurally distinct readings, each grounding technology legitimacy in a different primary criterion. This story instantiates the velocity-primacy reading only; sibling readings (reliability-primacy, precautionary) are separate constraint stories. The three readings coexist in contemporary climate policy; different coalitions hold different readings. Decomposition follows the ε-invariance principle: each reading instantiates a different beneficiary/victim structure and would compute different classifications from the same kernel text. Linking via affects_constraints establishes the family relationship and enables contamination propagation analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
