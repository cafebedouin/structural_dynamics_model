% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewables-Plus-Storage Decarbonization Legitimacy (Renewable Primacy Reading)
 *   domain: energy/climate/policy
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested
 *   'climate_mitigation_legitimacy' kernel: the claim that renewables plus
 *   storage can achieve full decarbonization faster and cheaper than nuclear.
 *   The reading's beneficiaries are distributed renewable developers, storage
 *   vendors, and rapid-decarbonization advocates; its victims are nuclear
 *   stakeholders, baseload-dependent regions, and long-cycle capital
 *   investors. The constraint is claimed as Tangled Rope: it carries a
 *   genuine coordination function (alignment on technology pathways for
 *   capital flow and deployment) AND asymmetric extraction (capital moves
 *   away from nuclear, costs shift to grid operators and baseload-dependent
 *   sectors). Suppression is high because the reading's persistence depends
 *   on actively filtering out alternative framings and data that would
 *   support competing readings (baseload necessity, portfolio pragmatism).
 *   Theater is elevated because advocacy coalitions perform 'renewable
 *   inevitability' while grid-integration questions remain unresolved. This
 *   is one of four sibling readings of the same kernel; the others
 *   (baseload_necessity_reading, portfolio_pragmatism_reading,
 *   degrowth_sufficiency_reading) represent institutional competitors with
 *   different beneficiary/victim structures and measurement boundaries.
 *
 * KEY AGENTS:
 *   - Distributed renewable developers (beneficiary, powerful, exit: mobile) — have incentive and capacity to advocate for renewable primacy; can shift investment to other sectors if blocked.
 *   - Storage technology vendors (beneficiary, organized, exit: mobile) — positions batteries/hydrogen as essential; funds research supporting scaling claims.
 *   - Climate urgency advocates (beneficiary, organized, exit: constrained) — NGOs and movements benefit from 'faster' narrative; politically invested; limited ability to shift priorities.
 *   - Nuclear industry stakeholders (payer, powerful, exit: trapped) — face capital starvation, delayed/cancelled projects, long timelines to pivot; geographically concentrated.
 *   - Baseload-dependent regions (payer, moderate, exit: constrained) — industrial heartlands, high winter demand regions, limited renewable resources; discover higher costs when renewable transition fails to deliver baseline power.
 *   - Long-cycle capital investors (payer, institutional, exit: constrained) — pension funds, development banks with 30-50yr horizons; face stranded assets from policy shifts.
 *   - Grid operators (excluded, institutional, exit: trapped) — TSOs responsible for dispatch and stability; not consulted in reading formulation; will bear operational cost of wrong choice.
 *   - Engineers/grid researchers (observer, analytical, exit: analytical) — provide empirical data; observations often selectively cited rather than driving consensus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.71).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewables-Plus-Storage Decarbonization Legitimacy (Renewable Primacy Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy/climate/policy").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '6404712a-3fd5-42d2-a5f8-1a47a1d0b18b').
narrative_ontology:cs_kernel_codification('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', distributed).
narrative_ontology:cs_authority_grounding('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', extraction).
narrative_ontology:cs_interpretation_layer_present('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b').
narrative_ontology:cs_reading_relation('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', foundational, storage_scalability_sufficient_for_full_decarbonization).
narrative_ontology:cs_axiom_status(storage_scalability_sufficient_for_full_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', storage_scalability_sufficient_for_full_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', foundational, renewable_cost_advantage_persists_including_system_integration).
narrative_ontology:cs_axiom_status(renewable_cost_advantage_persists_including_system_integration, holdable).
narrative_ontology:cs_axiom_grounding('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', renewable_cost_advantage_persists_including_system_integration, empirically_contingent).
narrative_ontology:cs_axiom('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', secondary, rapid_deployment_timelines_are_binding_constraint_on_climate_outcomes).
narrative_ontology:cs_axiom_status(rapid_deployment_timelines_are_binding_constraint_on_climate_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', rapid_deployment_timelines_are_binding_constraint_on_climate_outcomes, deontological).
narrative_ontology:cs_reference_frame('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', rapid_decarbonization_via_renewable_plus_storage_optimality).
narrative_ontology:cs_drift_state('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', contemporary_grid_integration_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6404712a-3fd5-42d2-a5f8-1a47a1d0b18b', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, storage_technology_vendors).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_urgency_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry_stakeholders).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_dependent_regions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, long_cycle_capital_investors).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, rapid_cost_decline_renewables).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, storage_scalability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar/wind/battery manufacturers and developers benefit directly from policy framing that privileges renewables-plus-storage as the decarbonization path. Their market expands, deployment accelerates, and capital flows to their sector. They have exit options (can serve other sectors or markets) but strong incentive to promote this reading through advisory, standards-setting, and policy engagement.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers, beneficiary,
    powerful, biographical, mobile, global).

% Battery, thermal storage, and hydrogen producers benefit from the reading's core claim that storage is the scalable solution. Their technologies are positioned as essential infrastructure. They actively shape policy narratives and fund research supporting storage scalability claims.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, storage_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% Climate NGOs, youth movements, and rapid-decarbonization advocates benefit from the reading's framing that decarbonization can be achieved without the 10-15 year construction timelines of nuclear. The reading aligns with the urgency narrative. Their stakes are ideological and political rather than financial, but substantial in terms of policy influence.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_urgency_advocates, beneficiary,
    organized, biographical, constrained, global).

% Nuclear utilities, reactor manufacturers, fuel suppliers, and skilled workforces bear the cost of this reading. Capital flows away from nuclear projects, policy support diminishes, and planned reactors are cancelled or delayed. Their long development timelines and asset lock-in make exit costly; many nuclear stakeholders are geographically concentrated in specific regions with limited alternative sectors.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry_stakeholders, payer,
    powerful, generational, trapped, global).

% Jurisdictions with high winter heating demand, industrial baseload requirements, or geography unsuitable for distributed renewables (northern latitudes, limited wind/solar resource) are told decarbonization will be renewable-led, then face grid stability or affordability challenges. They pay through either accepting higher renewable cost for equivalent decarbonization or discovering their transition timeline extends despite the reading's 'faster' claim.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_dependent_regions, payer,
    moderate, generational, constrained, regional).

% Pension funds, development banks, and long-term investors committed to nuclear projects face stranded assets, delayed returns, and policy shifts that reduce project viability. Their capital allocation horizons (30-50 years) match nuclear's development cycle, but this reading shifts policy toward shorter-cycle renewables, disrupting their portfolio planning.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, long_cycle_capital_investors, payer,
    institutional, generational, constrained, global).

% Grid operators (TSOs/system operators) are responsible for physical dispatch and stability but are not consulted in the policy framing of decarbonization pathways. They operate under constraints imposed by this reading but have limited voice in its formulation. Their operational reality (the feasibility omega) would inform the reading if they were included.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators, excluded,
    institutional, biographical, trapped, national).

% Communities whose land hosts renewable infrastructure or whose water resources are affected by hydro/cooling demands of any generation type are often excluded from decarbonization reading debates. This reading privileges rapid deployment without necessarily centering community consent or benefit-sharing; local voices would contest extraction dynamics at the site level.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, indigenous_and_local_communities, excluded,
    powerless, biographical, trapped, local).

% Independent researchers modeling grid integration, storage cost curves, and system reliability provide technical data consumed by multiple readings. They occupy an analytical seat and ideally constrain the readings toward empirical defensibility, though their findings are often selectively cited by advocacy coalitions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, engineers_and_grid_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: rapid decarbonization requires alignment on technology pathways, investment prioritization, and infrastructure buildout. Coordinating globally on renewable-plus-storage architecture addresses capital misallocation and investor confusion about which technologies are worth funding.
% TRANSFER_FUNCTION: Moves policy preference, research funding, and capital deployment from long-cycle nuclear projects to short-cycle renewable and storage projects. Transfers opportunity cost (foregone nuclear sector growth, baseload-industry revenue) to renewable beneficiaries. Transfers construction-risk and grid-integration costs to whoever operates the resulting variability-heavy systems.
% ABSENT_VOICES: Grid operators (TSO/system operators), baseload-dependent industrial users, and indigenous/local communities whose land and water serve renewable infrastructure buildout are structurally excluded or marginalized. They would object that the reading prioritizes speed and financial returns over grid stability, affordability for baseload-dependent sectors, and community benefit-sharing.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight — i.e., if policy reverted to technology-neutral decarbonization — capital flows would reorganize, nuclear projects shelved would be revived, renewable buildout would slow, grid planners would adopt different architectures for seasonal/duration storage. Decarbonization timelines would extend or diversify by region.
% FOUNDING_PROBLEM: Climate change requires rapid, cost-effective decarbonization; historical consensus viewed nuclear as essential baseload and renewables as supplementary. This reading emerged as renewable costs fell exponentially (2010-2025) and battery storage became technically feasible; it reframes decarbonization to center the fastest-deploying, cheapest-at-scale technologies.
% FOUNDING_PROBLEM_CORROBORATION: Renewable advocates (IRENA, BloombergNEF, some IPCC pathways) and cost analysts attest that the technical frontier has shifted and rapid renewable-plus-storage decarbonization is now feasible at lower cost. Nuclear industry, some energy economists, and grid operators attest that the founding problem persists — either storage is not yet proven at required scales, or grid integration costs neutralize renewable price advantage. Academic studies are divided; no consensus exists outside advocacy camps.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rose from 0.35 (2010) to 0.68 (2025) because the renewable cost curve and storage maturity claims moved from speculative to policy-normative — what was once a competitive hypothesis became a decision rule for capital allocation. Suppression rose from 0.4 to 0.71 over the same period as the constraint gained institutional backing (EU Green Deal, national net-zero commitments) and alternative readings were filtered from mainstream energy policy discourse. Theater rose from 0.25 to 0.42 as the constraint became more performative: renewable inevitability is asserted even where grid-integration or baseload studies suggest contingency. The metrics were authored at a shared time grid so every measurement appears at every time point examined; the interval spans 2010-2045 to capture the rise of the reading (2010-2025 observed) and its projected trajectory under continued policy lock-in (2025-2045 projected). The projection assumes the reading persists as policy orthodoxy without empirical resolution of the grid-stability omega.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (renewable developers, climate advocates), the constraint appears as coordination: alignment on the fastest-deploying, lowest-cost pathway, with extraction as a side effect of capital reallocation. From the payer seats (nuclear industry, baseload regions, grid operators), the constraint appears as pure extraction masked by coordination rhetoric: capital flight driven by advocacy coalitions and cost-curve extrapolation, with the cost of being wrong (grid instability, affordability failure, stranded renewable assets) borne by the latter. The engine should compute this divergence from the power atoms and exit options: beneficiaries have mobile exit, payers are trapped or constrained; the same constraint will classify differently across seats. The founder-problem_corroboration omega documents that no independent voice outside advocacy camps attests the reading's empirical claims are resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (distributed renewable developers, storage vendors) have d near 0.0-0.2 (full beneficiaries, mobile exit, powerful, no suppression felt). Climate urgency advocates have d near 0.1-0.3 (beneficiary, organized, but exit is ideological/political rather than economic). Nuclear stakeholders have d near 0.8-0.95 (full targets, trapped exit, powerful but powerless to redirect policy, bear extraction via capital starvation). Baseload-dependent regions have d near 0.6-0.75 (targets, constrained exit, moderate power, will discover costs downstream). Grid operators have d near 0.65-0.80 (targets without formal payer role, trapped, will absorb operational burden). Long-cycle investors have d near 0.75-0.85 (targets, constrained exit, institutional power insufficient to reverse policy flows). The directionality chain is derivable from beneficiary/victim declarations and exit options; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rapid decarbonization) is contested as 'dead' or 'live' depending on which reading adjudicates: the renewable reading asserts the problem is live and this pathway solves it; the baseload reading asserts the problem requires different tools; the portfolio reading asserts the problem requires both. The mandatrophy signal is NOT present yet because the reading's function (coordination on technology allocation) is still tied to its extraction (capital reallocation). If grid-integration fails, baseload shortages emerge, or storage costs remain prohibitive at scale, the constraint could flip toward Piton: coordination function dead, extraction persists via momentum and institutional inertia. That transition is captured by the theater-ratio rise and the grid-stability omega. The disappearance verdict (world_rearranges) confirms arrangements do depend on this reading's persistence, so mandatrophy is not yet resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grid_stability_empirical_frontier,
    'Can variable renewable generation plus storage physically achieve stable, dispatchable power at 100% decarbonization across diverse grid topologies (continental scale, seasonal storage depth, demand variability)?',
    'Multi-year grid-integration studies with real operational data from high-renewable jurisdictions; engineering constraints on storage duration/cost at required scales; modeling of edge cases (prolonged continental anticyclones, winter demand peaks in temperate climates).',
    'If unresolved negatively (storage costs remain prohibitive at required durations or stability cannot be achieved in practice), the reading''s core extraction claim (nuclear as capital sink) inverts — nuclear becomes necessary cost, not extractive distraction. If resolved positively, the reading''s structural classification remains tangled_rope but with lower extracted value; if resolved affirmatively with large margin, the constraint may compute as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_empirical_frontier, empirical, 'Technical feasibility of 100% renewable plus storage decarbonization at scale.').

omega_variable(
    kernel_framing_underspecification,
    'Does ''decarbonization'' in the founding mandate mean zero CO2 emissions, or economy-wide net-zero including offsets and carbon capture? Do ''faster'' and ''cheaper'' measure total system cost including storage and grid reinforcement, or generation-only costs?',
    'Examine the kernel''s original framing (climate policy documents, IPCC framings, national NDCs); if multiple framings coexist, declare which this reading adopts. Test whether sibling readings use different measurement boundaries.',
    'A reading that measures only marginal generation cost will show renewable advantage; one that includes full system integration cost (transmission, storage, grid hardening, backup capacity) may show parity or nuclear advantage. Measurement boundary determines the constraint''s ε and victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underspecification, conceptual, 'System boundary and cost accounting differences between readings.').

omega_variable(
    capital_cycle_temporal_bias,
    'Is the ''faster'' claim about time-to-operational deployment (shorter manufacturing/construction lead times for renewables), or time-to-climate-impact (when CO2 displacement begins flowing through the grid and atmosphere)?',
    'Trace the claim through policy documents and advocacy literature; compare deployment clock (nuclear: 10-15yr construction, renewables: 1-3yr) vs. system-integration clock (both: years of grid adaptation required to reach equivalent displacement). If advocates cite only deployment clock while grid integration equally delays impact, the claim is misleading.',
    'A true ''faster'' finding strengthens the extraction claim (nuclear delays decarbonization timeline). A finding that both technologies face equivalent system-integration delays weakens it — the bottleneck is grid, not nuclear capital per se. This omega addresses whether the reading''s victim-set classification (nuclear as bottleneck) is structurally defensible or rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_cycle_temporal_bias, conceptual, 'Temporal measurement scope bias in the ''faster'' claim.').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does the renewable primacy reading''s core empirical claim — storage costs + renewable cost curves enable 100% decarbonization cheaper than nuclear — logically foreclose the baseload_necessity_reading (which holds baseload is technically necessary), or do both remain live depending on grid topology and storage technology maturity?',
    'Examine the baseload reading''s core: if it asserts baseload is INHERENTLY necessary (immutable physics), the readings foreclose each other; if it asserts baseload is necessary under CURRENT technology and costs, both remain live (storage maturity is the differentiator). If the baseload reading''s reference frame is ''contemporary grid'' and this reading''s is ''future-mature-storage grid,'' they coexist across time rather than foreclosing.',
    'If foreclosed, this reading''s cs_structure.reading_relations should show forecloses:baseload_necessity_reading. If coexisting, the relation is coexists_with (different scenarios/time horizons). This determination shapes whether the kernel is resolvable-by-facts or permanently contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Whether sibling readings occupy different scenarios/time horizons or logically eliminate each other.').

omega_variable(
    authority_grounding_shift_dynamics,
    'Who is the authority that adjudicates which reading is legitimate? Is it engineering consensus (IPCC, national grid operators, peer-reviewed modeling), policy bodies (energy ministries, regulatory commissions), or advocacy coalitions (climate NGOs, investor networks)?',
    'Map which institutional seats endorse each reading; examine how authority shifted from 2010-2025 (engineering consensus on feasibility was lower; cost data was favorable to nuclear). Document whether this reading has gained authority through empirical maturation (falling battery costs, operational data from high-RE grids) or through advocacy-coalition mobilization (political shifts in funding priorities).',
    'If this reading has gained authority through maturation of falsifiable empirical claims (storage cost curves, grid integration data), the extraction claim is stronger — nuclear is being delayed by legitimacy deficits in policy despite remaining technically defensible. If authority shifted through coalition mobilization without empirical resolution, the constraint may be political extraction masked as technical claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_shift_dynamics, empirical, 'Authority grounding changes and their drivers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 2010, 2045).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement_basis(clim_tr_t2010, observed).
narrative_ontology:measurement(clim_tr_t2018, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2018, 0.32).
narrative_ontology:measurement_basis(clim_tr_t2018, observed).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2025, observed).
narrative_ontology:measurement(clim_tr_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2035, 0.46).
narrative_ontology:measurement_basis(clim_tr_t2035, projected).
narrative_ontology:measurement(clim_tr_t2045, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2045, 0.43).
narrative_ontology:measurement_basis(clim_tr_t2045, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement_basis(clim_be_t2010, observed).
narrative_ontology:measurement(clim_be_t2018, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement_basis(clim_be_t2018, observed).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(clim_be_t2025, observed).
narrative_ontology:measurement(clim_be_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2035, 0.71).
narrative_ontology:measurement_basis(clim_be_t2035, projected).
narrative_ontology:measurement(clim_be_t2045, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2045, 0.68).
narrative_ontology:measurement_basis(clim_be_t2045, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement_basis(clim_su_t2010, observed).
narrative_ontology:measurement(clim_su_t2018, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement_basis(clim_su_t2018, observed).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(clim_su_t2025, observed).
narrative_ontology:measurement(clim_su_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2035, 0.73).
narrative_ontology:measurement_basis(clim_su_t2035, projected).
narrative_ontology:measurement(clim_su_t2045, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2045, 0.7).
narrative_ontology:measurement_basis(clim_su_t2045, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__renewable_primacy_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% The renewable_primacy_reading is one of four structurally distinct readings of the climate_mitigation_legitimacy kernel. Each reading has different beneficiary/victim sets, measurement boundaries, and extraction profiles. The sibling readings (baseload_necessity, portfolio_pragmatism, degrowth_sufficiency) are not alternative measurements of the same constraint; they are separate constraints with their own ε values and stakeholder situations. This story's network links establish that each reading structurally influences the others' policy viability and capital flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__renewable_primacy_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
