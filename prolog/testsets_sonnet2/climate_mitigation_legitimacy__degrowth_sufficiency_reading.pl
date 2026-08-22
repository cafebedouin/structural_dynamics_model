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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Sufficiency/Degrowth Reading of Climate Mitigation Legitimacy
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates the degrowth/sufficiency reading of the contested
 *   climate_mitigation_legitimacy kernel: the claim that decarbonization is
 *   best achieved primarily through absolute demand reduction, making
 *   large-scale generation capacity expansion — whether nuclear or renewable
 *   — substantially unnecessary. This is a distinct constraint from the
 *   sibling readings (baseload_necessity, renewable_primacy,
 *   portfolio_pragmatism), each of which authors a different
 *   beneficiary/victim structure and a different ε from the same underlying
 *   kernel text (the legitimacy of a decarbonization pathway). Under this
 *   reading specifically, both nuclear and utility-scale renewable developers
 *   land in the victim set, because both represent the capital-intensive
 *   supply expansion the sufficiency framework treats as the wrong lever. The
 *   reading's coordination function is real (demand-side mitigation reduces
 *   required build-out) but its enforcement — via funding conditionality,
 *   scenario-model gatekeeping, and advocacy pressure on financing bodies —
 *   imposes disproportionate costs on populations needing capacity growth for
 *   development, which is why it is authored as tangled_rope rather than pure
 *   rope.
 *
 * KEY AGENTS:
 *   - degrowth_policy_advocates: agenda-setting seat administering the sufficiency framework's intellectual and institutional infrastructure
 *   - nuclear_developers and utility_scale_renewable_developers: both classified as growth-dependent victims under this specific reading
 *   - energy_poor_households_in_developing_regions: bear the sharpest cost — trapped exit, powerless, generational time horizon
 *   - climate_scientists_ipcc_working_group_iii: analytical observer seat assessing pathway plausibility across all four sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.42).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Sufficiency/Degrowth Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '3380b672-e251-45e9-b589-fc0089e62342').
narrative_ontology:cs_kernel_codification('3380b672-e251-45e9-b589-fc0089e62342', distributed).
narrative_ontology:cs_authority_grounding('3380b672-e251-45e9-b589-fc0089e62342', distributed).
narrative_ontology:cs_reading_relation('3380b672-e251-45e9-b589-fc0089e62342', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3380b672-e251-45e9-b589-fc0089e62342', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3380b672-e251-45e9-b589-fc0089e62342', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_axiom('3380b672-e251-45e9-b589-fc0089e62342', foundational, aggregate_throughput_reduction_is_the_primary_lever).
narrative_ontology:cs_axiom_status(aggregate_throughput_reduction_is_the_primary_lever, holdable).
narrative_ontology:cs_axiom_grounding('3380b672-e251-45e9-b589-fc0089e62342', aggregate_throughput_reduction_is_the_primary_lever, empirically_contingent).
narrative_ontology:cs_axiom('3380b672-e251-45e9-b589-fc0089e62342', foundational, large_scale_capital_deployment_is_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(large_scale_capital_deployment_is_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('3380b672-e251-45e9-b589-fc0089e62342', large_scale_capital_deployment_is_presumptively_illegitimate, instrumental).
narrative_ontology:cs_reference_frame('3380b672-e251-45e9-b589-fc0089e62342', post_growth_ecological_limits_framework).
narrative_ontology:cs_drift_state('3380b672-e251-45e9-b589-fc0089e62342', contemporary_energy_transition_financing_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('3380b672-e251-45e9-b589-fc0089e62342', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_policy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, demand_reduction_ngo_sector).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_research_institutes).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_dependent_manufacturing_workers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_households_in_developing_regions).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, planetary_boundaries_thesis).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_economics_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and promote the sufficiency framework in IPCC working-group commentary, academic journals, and NGO advocacy: decarbonization pathways should prioritize absolute demand reduction (fewer flights, smaller homes, less industrial throughput) over building new generation capacity of any kind. They set the terms of the debate within sufficiency-adjacent policy circles and administer the intellectual infrastructure — journals, conference tracks, model assumptions in scenario literature — that decides which pathways count as legitimate.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_policy_advocates, agenda_setter,
    organized, civilizational, analytical, global).

% Receive funding, staffing, and institutional standing premised on demand-side mitigation being the primary legitimate lever. Their programmatic existence depends on the sufficiency framing continuing to be treated as centrally important rather than as one input among several supply-side and demand-side options.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, demand_reduction_ngo_sector, beneficiary,
    organized, generational, constrained, national).

% Academic centers and think tanks whose modeling output and grant pipelines are built around demonstrating that supply-side expansion (nuclear or renewable buildout at scale) is unnecessary if demand falls fast enough. Their scholarly reputations and funding streams are tied to the sufficiency conclusion holding up in scenario comparisons.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_research_institutes, beneficiary,
    institutional, generational, constrained, global).

% Capital-intensive projects requiring decades-long financing and political commitment are recast, under the sufficiency reading, as unnecessary or actively counterproductive because their construction footprint and long lead times represent exactly the kind of large-scale capital deployment the reading argues against. Financing and permitting environments shift against them when sufficiency framing dominates public discourse and multilateral funding criteria; exit means relocating projects to jurisdictions where the framing has less purchase.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_developers, payer,
    powerful, generational, constrained, national).

% Also classified under this reading as growth-dependent: massive wind and solar buildout, transmission expansion, and storage deployment are treated as extensions of the same expansionary paradigm the sufficiency reading rejects, even though they displace fossil generation. Developers lose access to sufficiency-aligned funding streams and face advocacy pressure characterizing large renewable projects as land-use and materials-throughput problems rather than climate solutions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewable_developers, payer,
    powerful, biographical, constrained, continental).

% Employment in energy-intensive manufacturing (steel, cement, chemicals, EV battery supply chains) depends on continued grid capacity growth to electrify processes. Under a sufficiency-first policy regime that treats capacity expansion as illegitimate, planned electrification investment stalls, threatening job continuity. Workers have no voice in the framework debate and cannot relocate their industries on short notice.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_dependent_manufacturing_workers, payer,
    powerless, biographical, trapped, regional).

% Populations with per-capita energy consumption far below sufficiency thresholds calculated from wealthy-country baselines are nonetheless folded into global demand-reduction targets and financing conditionalities that discourage new generation investment in their countries. Their exit options are nonexistent: they depend on multilateral and bilateral financing that is increasingly gated by sufficiency-compliant development criteria.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_households_in_developing_regions, payer,
    powerless, generational, trapped, global).

% Assess competing decarbonization pathway models (including sufficiency, renewable-primacy, and portfolio scenarios) for physical and economic plausibility, and note in assessment reports where sufficiency assumptions embed contestable social and political premises not derivable from physical constraints alone.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_scientists_ipcc_working_group_iii, observer,
    institutional, civilizational, analytical, global).

% Regions currently under-electrified — rural areas, rapidly growing cities in the Global South — that need MORE generation capacity to achieve basic development goals are rarely present in the sufficiency literature's own case studies, which draw predominantly on over-consuming wealthy-country contexts. Their development trajectory would be directly foreclosed by a sufficiency-first global framework, but they are not represented in the advocacy or research bodies setting the agenda.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_expansion_dependent_regions, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine and real problem: aggregate material and energy throughput in wealthy economies is difficult to decarbonize fast enough through supply substitution alone, and demand-side measures (efficiency, modal shift, consumption moderation) can meaningfully reduce the total capacity that needs to be built, financed, and sited — a real coordination gain when done well.
% TRANSFER_FUNCTION: Moves legitimacy, funding priority, and multilateral financing conditionality away from large capital-intensive generation projects (both nuclear and utility-scale renewables) and toward demand-reduction programs and the institutions that administer them, while imposing the foregone-capacity cost on populations and industries that depend on capacity growth to develop or to decarbonize energy-intensive processes.
% ABSENT_VOICES: Under-electrified regions and communities in the Global South needing capacity growth for basic development are structurally absent from the advocacy and research institutions that set sufficiency criteria, which are disproportionately staffed and funded from over-consuming wealthy-country contexts; energy-intensive manufacturing workers whose jobs depend on electrification investment are also not represented in the framework debate.
% DISAPPEARANCE_RATIONALE: If the sufficiency reading vanished overnight as an organizing framework, degrowth-aligned research institutes and NGOs would lose funding and standing (their world clearly rearranges), while nuclear and renewable developers would face a friendlier financing and permitting environment. Whether the underlying world — actual emissions trajectories, actual development outcomes — would meaningfully change is disputed: proponents argue demand reduction is doing real decarbonization work that supply substitution cannot replicate fast enough; critics argue the framework mainly gates capital deployment without proportionate emissions benefit, especially in under-electrified regions.
% FOUNDING_PROBLEM: Rising aggregate energy and material consumption in industrialized economies threatens to outpace the physical and financial feasibility of supply-side decarbonization alone, especially given land, mineral, and grid-buildout constraints on both nuclear and renewable expansion at the pace required.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists and some IPCC WG3 chapter contributors outside the sufficiency-advocacy sector corroborate that demand-side measures carry real mitigation potential documented in the assessment literature. However, energy-access researchers and development economists outside the sufficiency research community counter-attest that applying wealthy-country sufficiency thresholds globally would foreclose legitimate development needs in under-electrified regions, and grid engineers note that many manufacturing decarbonization pathways require electrification growth the sufficiency framing treats as illegitimate — corroboration for the founding problem's continued live status is contested precisely along the axis the reading itself organizes discourse around.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is mid-high: the sufficiency reading extracts institutional standing and financing priority from two large capital classes (nuclear and renewable developers) simultaneously, which is a wider extraction footprint than either the baseload_necessity or renewable_primacy readings would author (each of which victimizes only one technology class). Suppression is moderate (0.42) — the mechanism is agenda-setting and financing conditionality rather than legal coercion, so it is real but softer than direct enforcement. Resistance is high (0.72) because both excluded technology sectors are powerful, well-resourced actors who actively contest the framing in policy and financing arenas. Accessibility collapse is comparatively low (0.35) — the sufficiency reading has not achieved anything close to monopoly control over decarbonization discourse; portfolio and renewable-primacy readings remain fully live alternatives most policymakers can and do adopt instead.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this reading is coordination: preventing wasteful overbuild that would lock in unnecessary material and mineral extraction. From the payer seats — both technology-developer classes and energy-poor populations — the same structure computes as extraction: legitimate capital plans and development pathways foreclosed by a framework calibrated to a different context (wealthy-country overconsumption) than the one being constrained (their own capacity needs).
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth advocates and sufficiency research institutes sit at the beneficiary end: the framework's dominance is their institutional currency. Nuclear and renewable developers sit at the target end: their capital plans lose legitimacy and financing access when this reading gains ground, despite both representing genuine decarbonization pathways under sibling readings. Energy-poor households in developing regions are the most extreme target case — trapped, powerless, generational harm — because sufficiency criteria calibrated to wealthy-country overconsumption are applied to populations who have not yet reached basic energy adequacy, inverting the reading's own justification (curbing excess) into a constraint on necessary growth.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (finite ecological headroom for aggregate throughput growth) remains partially live in wealthy-country contexts but is contested when extended globally — the founding_problem_status is authored as contested rather than dead or live because the reading's applicability genuinely varies by context: sufficiency has real purchase against overconsumption in wealthy nations but produces a category error when applied to energy-poor regions still below basic sufficiency thresholds. Classifying this as tangled_rope rather than snare acknowledges the reading's genuine coordination content (demand reduction is real mitigation, not merely cover) while flagging that its financing-gatekeeping enforcement imposes costs beyond what the coordination function alone would justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_threshold_universality,
    'Is the sufficiency threshold used to judge ''unnecessary generation expansion'' a universal physical/ecological limit, or a wealthy-country-calibrated social construct being applied globally?',
    'Compare per-capita energy consumption thresholds used in sufficiency literature against basic development needs (health, education, industrialization) benchmarks in under-electrified regions; audit whether sufficiency case studies and datasets are geographically representative or skewed toward OECD contexts.',
    'If the threshold is a wealthy-country construct, applying it globally reclassifies necessary development capacity as illegitimate excess, sharply increasing the reading''s extractive footprint on developing-region victims. If genuinely universal (grounded in planetary boundary physics independent of development status), the extraction on those populations would be better understood as a shared physical constraint rather than an institutional imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_threshold_universality, conceptual, 'Whether sufficiency thresholds are physically universal or geographically parochial.').

omega_variable(
    demand_reduction_feasibility_at_required_pace,
    'Can demand reduction alone, without large-scale generation expansion, actually achieve decarbonization at the pace required by remaining carbon budgets, or does the sufficiency pathway implicitly rely on supply substitution it declines to fund?',
    'Integrated assessment model comparison: run sufficiency-only pathways against observed historical rates of demand reduction achieved through policy, and check whether required rates are empirically achieved anywhere at the necessary scale and speed.',
    'If demand reduction alone cannot close the gap at required pace, the sufficiency reading''s opposition to generation expansion would itself slow decarbonization, undermining its own founding justification and shifting classification toward a more purely extractive reading (institutional capture of the decarbonization narrative without proportionate mitigation delivery).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_reduction_feasibility_at_required_pace, empirical, 'Whether the sufficiency pathway is physically adequate on its own terms.').

omega_variable(
    reading_selection_evidentiary_basis,
    'Given four live sibling readings of the same kernel (baseload_necessity, degrowth_sufficiency, portfolio_pragmatism, renewable_primacy), what evidentiary or normative criteria would a neutral observer use to prefer one reading''s legitimacy claim over another, and does this reading''s institutional dominance in some policy venues reflect the strength of its evidence or the strength of its advocacy infrastructure?',
    'Independent meta-analysis of decarbonization pathway literature, controlling for institutional funding source and advocacy affiliation of study authors, comparing predictive accuracy of each reading''s prior claims against realized emissions and capacity outcomes.',
    'If institutional advocacy strength rather than evidentiary strength explains this reading''s influence in specific venues, the classification shifts further toward tangled_rope/snare (narrative capture); if evidentiary strength explains it, the coordination function is more robustly grounded and the classification would sit closer to rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Whether this reading''s institutional purchase tracks evidence or advocacy capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the single natural-language label 'climate mitigation legitimacy' per the epsilon-invariance principle. Each reading of the kernel (baseload_necessity, degrowth_sufficiency, portfolio_pragmatism, renewable_primacy) authors a different beneficiary/victim structure and a different epsilon, because each reading contests a different claim about which decarbonization pathway is legitimate. The degrowth_sufficiency_reading (this file) is structurally distinctive in placing both nuclear and utility-scale renewables in the victim set simultaneously — sibling readings place at most one technology class in the victim set. All four are linked bidirectionally via affects_constraints because a shift in institutional dominance of any one reading directly changes financing and legitimacy conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
