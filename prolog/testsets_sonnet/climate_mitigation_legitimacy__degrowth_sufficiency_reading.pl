% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth-Sufficiency Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested 'climate mitigation
 *   legitimacy' kernel: the claim that genuine decarbonization requires
 *   demand reduction sufficient to make large-scale generation expansion —
 *   nuclear or renewable — unnecessary. Unlike the baseload-necessity,
 *   renewable-primacy, or portfolio-pragmatism readings, this reading does
 *   not pick a winning generation technology; it disputes the premise that
 *   generation expansion of any kind is the correct unit of decarbonization
 *   progress. Both nuclear and renewable developers therefore enter this
 *   reading's victim set as growth-dependent actors whose legitimacy is
 *   undercut by the same standard, which is the structural delta
 *   distinguishing this reading from its three siblings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.44).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth-Sufficiency Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e9eeee1c-a466-4223-85ce-727d09c1a471').
narrative_ontology:cs_kernel_codification('e9eeee1c-a466-4223-85ce-727d09c1a471', distributed).
narrative_ontology:cs_authority_grounding('e9eeee1c-a466-4223-85ce-727d09c1a471', distributed).
narrative_ontology:cs_reading_relation('e9eeee1c-a466-4223-85ce-727d09c1a471', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9eeee1c-a466-4223-85ce-727d09c1a471', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9eeee1c-a466-4223-85ce-727d09c1a471', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_axiom('e9eeee1c-a466-4223-85ce-727d09c1a471', foundational, generation_expansion_is_not_the_correct_unit_of_progress).
narrative_ontology:cs_axiom_status(generation_expansion_is_not_the_correct_unit_of_progress, holdable).
narrative_ontology:cs_axiom_grounding('e9eeee1c-a466-4223-85ce-727d09c1a471', generation_expansion_is_not_the_correct_unit_of_progress, empirically_contingent).
narrative_ontology:cs_axiom('e9eeee1c-a466-4223-85ce-727d09c1a471', secondary, demand_contraction_is_achievable_at_required_scale).
narrative_ontology:cs_axiom_status(demand_contraction_is_achievable_at_required_scale, holdable).
narrative_ontology:cs_axiom_grounding('e9eeee1c-a466-4223-85ce-727d09c1a471', demand_contraction_is_achievable_at_required_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('e9eeee1c-a466-4223-85ce-727d09c1a471', supply_side_buildout_as_default_decarbonization_metric).
narrative_ontology:cs_drift_state('e9eeee1c-a466-4223-85ce-727d09c1a471', post_2020s_deployment_delay_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e9eeee1c-a466-4223-85ce-727d09c1a471', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_policy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, demand_reduction_ngos).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_research_institutes).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poverty_reframing_coalitions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_capital_investors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_manufacturing_workers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, electrification_dependent_low_income_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legitimacy criterion that decarbonization pathways must be judged on demand-reduction credentials rather than supply-side buildout. Drafts model scenarios and policy submissions in which demand contraction substitutes for new generation capacity, and campaigns to have this framing adopted in climate plans and funding criteria. Bears little direct cost if the framing is adopted or rejected; can pivot rhetoric to other venues.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_policy_advocates, agenda_setter,
    organized, generational, mobile, national).

% Receives funding, mandates, and institutional standing that grow when demand-reduction is treated as the legitimate decarbonization pathway rather than one option among several. Programs, grants, and staff positions are structured around sufficiency framing being institutionally privileged.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, demand_reduction_ngos, beneficiary,
    organized, biographical, mobile, national).

% Produces the modeling scenarios (integrated assessment runs with steep demand contraction) that anchor the sufficiency framing's technical credibility. Research funding, citation counts, and policy influence expand when sufficiency-first scenarios are treated as authoritative inputs to national climate plans.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_research_institutes, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_research_institutes, agenda_setter).

% Requires long-lead-time capital commitments (a decade or more) justified against projected future demand. When national climate plans adopt a demand-reduction-first legitimacy standard, project justifications weaken, financing terms worsen, and permitting/funding priority shifts toward efficiency programs. Cannot easily relocate stranded capital or restart cancelled projects.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_developers, payer,
    powerful, generational, constrained, national).

% Depends on continued large-scale buildout mandates and capacity auctions for revenue. Where demand-reduction framing substitutes for capacity expansion in official planning, auction volumes shrink and grid connection queues deprioritize new renewable projects in favor of efficiency retrofits. Developers can shift capital to other jurisdictions but at a cost to sunk project pipelines.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_developers, payer,
    powerful, biographical, constrained, national).

% Holds portfolios priced on projected electricity demand growth (grid expansion, storage, transmission). A legitimacy standard that treats demand reduction as sufficient depresses forecast growth curves used in regulatory rate cases, reducing the asset base investors can recover returns on. Capital is mobile across sectors but existing grid-specific commitments are sunk.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_capital_investors, payer,
    powerful, biographical, mobile, continental).

% Employment depends on continued industrial throughput that demand-reduction policy explicitly targets for contraction (steel, aluminum, cement, chemicals). When sufficiency framing drives production quotas or energy rationing policy, job losses concentrate in these sectors with few comparable regional alternatives. Cannot easily retrain or relocate on the policy's timeline.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_manufacturing_workers, payer,
    powerless, biographical, trapped, regional).

% Needs continued and expanding electricity access for heating, cooking, and vehicle electrification as a path out of energy poverty. Where demand-reduction legitimacy suppresses new generation and grid capacity, these households face rationing, higher marginal prices, or delayed access to the electrification that would otherwise improve their material conditions. Cannot generate their own supply or exit the grid they depend on.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, electrification_dependent_low_income_households, payer,
    powerless, immediate, trapped, regional).

% Evaluates competing decarbonization pathway scenarios against emissions budgets and physical feasibility constraints, without institutional stake in which technology or demand pathway wins. Testifies in policy processes and publishes comparative scenario analyses that other seats cite selectively to support their preferred reading.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_scientists_and_iea_modelers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared standard for judging climate policy legitimacy: if aggregate energy demand is reduced through efficiency, behavioral change, and de-industrialization of energy-intensive sectors, the scale of new generation capacity (nuclear or renewable) required for decarbonization shrinks, avoiding the land-use, materials, financing, and siting conflicts that large buildouts create.
% TRANSFER_FUNCTION: Moves institutional legitimacy, research funding, and policy priority away from generation-expansion projects (nuclear and renewable) and toward demand-reduction programs and the organizations that administer them; moves material costs of contraction onto energy-intensive industrial workers and onto low-income households whose electrification access depends on capacity growth.
% ABSENT_VOICES: Energy-intensive manufacturing workers and low-income households seeking electrification access are rarely present in the technical modeling forums where sufficiency scenarios are debated; their objections surface mainly through labor unions and energy-poverty advocacy groups operating outside the climate-modeling community that sets the legitimacy standard.
% DISAPPEARANCE_RATIONALE: If the degrowth-sufficiency legitimacy standard disappeared overnight, national climate plans would revert to technology-neutral or supply-expansion framings, capacity auctions and nuclear financing pipelines would resume prior trajectories, and demand-reduction NGOs and sufficiency research institutes would lose the institutional standing built around this framing being treated as authoritative rather than as one input among several.
% FOUNDING_PROBLEM: Large-scale generation buildout (nuclear siting conflicts, renewable land-use and materials constraints, transmission bottlenecks) was proving slower and more contested than emissions timelines allow, and it appeared that reducing the scale of the problem through demand-side contraction could close the gap without waiting on supply-side buildout that repeatedly missed deployment targets.
% FOUNDING_PROBLEM_CORROBORATION: Degrowth advocates and sufficiency researchers themselves attest the founding problem (buildout delay) is real and worsening. Independent bodies outside this coalition — the IEA's net-zero scenario work and grid operators' capacity adequacy assessments — corroborate that generation buildout has lagged targets, but dispute the inference that demand reduction at the scale required is achievable or that it should set the legitimacy standard rather than supplement supply expansion; several assessments explicitly warn that sufficiency-first framing understates near-term electrification needs for energy-poor populations.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).
:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.44) and suppression (0.38) are moderate: this is a legitimacy-standard contest fought through modeling scenarios, funding criteria, and planning documents rather than through direct coercive enforcement, but it has real distributive teeth — capacity auctions, financing terms, and industrial policy shift when the standard is adopted. Theater ratio (0.28) reflects that a meaningful share of sufficiency advocacy is genuine technical modeling work, not pure performance, though rising over the interval as the framing becomes more institutionally entrenched and less continuously re-justified against evidence. Resistance is high (0.71) because both nuclear and renewable industries, along with industrial labor and energy-poverty advocates, actively contest the framing rather than acquiescing to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth advocates, demand-reduction NGOs, and sufficiency research institutes are structural beneficiaries: their institutional standing, funding, and policy relevance grow specifically when demand-reduction is treated as the legitimate decarbonization pathway rather than a complement to supply expansion. Nuclear developers, renewable developers, and grid capital investors are targets despite being on 'different sides' of the generation-technology debate — this reading's distinguishing move is to extract legitimacy from both simultaneously, which is why both enter the victim set together rather than one benefiting at the other's expense. Energy-intensive manufacturing workers and electrification-dependent low-income households are the most severely targeted: trapped exit options, immediate time horizons, and no capacity to generate substitute supply themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — buildout delay against emissions timelines — was genuinely live when the sufficiency framing gained institutional traction, and remains partially live today (siting and permitting delays persist for both nuclear and renewables). This prevents dismissing the reading as pure mandatrophy: the coordination function (avoiding buildout conflicts through demand contraction) is real, not fabricated. But the founding_problem_status is authored as contested rather than dead precisely because the corroboration is split — IEA and grid-operator assessments confirm the delay problem while disputing that sufficiency at the scale required is achievable, which is the seam where legitimate coordination shades into extraction against electrification-dependent households who bear immediate material costs for a demand-reduction trajectory whose feasibility outside modeling scenarios remains unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_feasibility_at_required_scale,
    'Can demand reduction actually be achieved at the scale and speed this reading requires to obviate generation expansion, or does the reading''s legitimacy claim outrun what behavioral and industrial contraction can deliver within emissions timelines?',
    'Longitudinal tracking of realized demand reduction against sufficiency-scenario projections in jurisdictions that have adopted demand-reduction-first climate plans, compared against counterfactual jurisdictions pursuing supply expansion.',
    'If sufficiency at the required scale proves infeasible, the reading functions primarily as legitimacy extraction from generation developers without delivering the emissions outcome it claims to enable, strengthening the tangled_rope classification toward snare; if feasible, the coordination function dominates and the classification should move toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_feasibility_at_required_scale, empirical, 'Whether the degrowth-sufficiency pathway is physically and politically achievable at the scale claimed.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Which institutional process determines which of the four kernel readings (baseload_necessity, degrowth_sufficiency, portfolio_pragmatism, renewable_primacy) becomes the operative legitimacy standard in a given jurisdiction''s climate policy, and is that selection process itself contestable on grounds independent of the readings'' technical merits?',
    'Comparative institutional analysis of how national climate plans adopt, blend, or reject each reading, tracing the advocacy coalitions and modeling bodies that shape adoption.',
    'If reading selection tracks institutional advocacy capacity more than technical merit, all four readings should be understood partly as legitimacy-capture contests rather than purely technical disagreements, which would raise the extractiveness estimate for this reading and its siblings alike.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether kernel-reading adoption is technically or politically determined.').

omega_variable(
    energy_poverty_tradeoff_weighting,
    'How should the framework weigh near-term electrification access for energy-poor households against long-horizon emissions reduction when the two pull in different directions under this reading?',
    'This is fundamentally a distributive-justice question about intergenerational and intragenerational tradeoffs, not resolvable by additional data alone — it depends on normative commitments about whose welfare counts and on what timescale.',
    'A framework privileging near-term energy access would classify this reading as more extractive toward low-income households; a framework privileging long-horizon emissions reduction would treat the same cost as a justified transitional burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_poverty_tradeoff_weighting, preference, 'Normative weighting of near-term energy access against long-horizon decarbonization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the climate_mitigation_legitimacy kernel, each with its own ε, beneficiary/victim structure, and classification. baseload_necessity_reading and renewable_primacy_reading both treat generation expansion as necessary but disagree on technology; portfolio_pragmatism_reading treats both technologies as complementary; this reading uniquely treats generation expansion itself as the target for minimization, placing both nuclear and renewable developers in its victim set. The four readings are linked via affects_constraints because adoption of any one reading as the operative legitimacy standard directly changes the resource availability and political legitimacy conditions the other three readings compete under.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
