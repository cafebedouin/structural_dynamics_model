% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewables-Primacy Doctrine in Climate Mitigation Policy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The claim that renewables plus storage can achieve full decarbonization
 *   faster and cheaper than nuclear began as an empirical proposition and
 *   hardened, over roughly three decades, into an operative legitimacy
 *   structure in climate and energy policy: it decides which technologies
 *   appear in 'serious' climate plans, which projects reach subsidized
 *   capital, which plants retire as supposed economic inevitabilities, and
 *   which advocacy positions count as credible. This story instantiates ONE
 *   reading of the contested climate_mitigation_legitimacy kernel — the
 *   renewable_primacy_reading — and generates it as a clean,
 *   epsilon-invariant constraint: the doctrine-as-operating, with its own
 *   stable beneficiary/victim structure. The sibling readings
 *   (baseload_necessity, portfolio_pragmatism, degrowth_sufficiency) are
 *   separate constraints in separate files; per the epsilon-invariance
 *   principle they carry different victim sets and different epsilon over the
 *   same policy terrain, and are linked through network.affects_constraints.
 *   Epsilon's referent here is the standing arrangement under contest — the
 *   subsidy, taxonomy, and discourse regime in which this claim currently
 *   gates legitimacy — assessed by this reading's own lights: the reading
 *   holds the claim substantially true, which is why the authored epsilon is
 *   moderate-high (real overreach at the margins: premature closures,
 *   gas-filled gaps, identity-rigid enforcement) rather than maximal.
 *
 * KEY AGENTS:
 *   - renewable_energy_developers: Primary beneficiary (institutional/arbitrage) — receives the directed capital flows the doctrine steers
 *   - climate_advocacy_organizations: Agenda setter with fused identity (organized/identity_locked) — administers pathway legitimacy through campaigns and scorecards
 *   - nuclear_energy_industry: Primary target (institutional/constrained) — bears asset stranding, closure losses, and financing exclusion
 *   - electricity_ratepayers: Diffuse target (moderate/trapped) — bears tariff pass-throughs where firm-clean options were foreclosed
 *   - nuclear_plant_host_communities: Localized target (powerless/constrained) — lose payroll and tax base to distant decisions
 *   - fossil_gas_operators: Incidental beneficiary (institutional/arbitrage) — fills the firm-capacity gaps the sequencing leaves open
 *   - national_grid_operators: Institutional target-observer (institutional/constrained) — absorb reliability and curtailment costs, publish the operational record
 *   - energy_poor_households: Excluded voice (powerless/trapped) — bear regressive pass-throughs with no seat in pathway proceedings
 *   - independent_energy_system_modelers: Analytical observer (analytical/analytical) — produce the firm-power findings both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewables-Primacy Doctrine in Climate Mitigation Policy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '777df0bd-7787-4782-b06c-1f47fbe7bb27').
narrative_ontology:cs_kernel_codification('777df0bd-7787-4782-b06c-1f47fbe7bb27', distributed).
narrative_ontology:cs_authority_grounding('777df0bd-7787-4782-b06c-1f47fbe7bb27', diffuse_epistemic).
narrative_ontology:cs_reading_relation('777df0bd-7787-4782-b06c-1f47fbe7bb27', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('777df0bd-7787-4782-b06c-1f47fbe7bb27', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('777df0bd-7787-4782-b06c-1f47fbe7bb27', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('777df0bd-7787-4782-b06c-1f47fbe7bb27', foundational, learning_curve_cost_dominance).
narrative_ontology:cs_axiom_status(learning_curve_cost_dominance, holdable).
narrative_ontology:cs_axiom_grounding('777df0bd-7787-4782-b06c-1f47fbe7bb27', learning_curve_cost_dominance, empirically_contingent).
narrative_ontology:cs_axiom('777df0bd-7787-4782-b06c-1f47fbe7bb27', foundational, storage_flexibility_firm_power_substitutability).
narrative_ontology:cs_axiom_status(storage_flexibility_firm_power_substitutability, holdable).
narrative_ontology:cs_axiom_grounding('777df0bd-7787-4782-b06c-1f47fbe7bb27', storage_flexibility_firm_power_substitutability, empirically_contingent).
narrative_ontology:cs_axiom('777df0bd-7787-4782-b06c-1f47fbe7bb27', secondary, distributed_generation_structural_privilege).
narrative_ontology:cs_axiom_status(distributed_generation_structural_privilege, holdable).
narrative_ontology:cs_axiom_grounding('777df0bd-7787-4782-b06c-1f47fbe7bb27', distributed_generation_structural_privilege, instrumental).
narrative_ontology:cs_reference_frame('777df0bd-7787-4782-b06c-1f47fbe7bb27', least_cost_rapid_renewables_pathway).
narrative_ontology:cs_drift_state('777df0bd-7787-4782-b06c-1f47fbe7bb27', contemporary_post_2022_energy_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('777df0bd-7787-4782-b06c-1f47fbe7bb27', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_gas_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_energy_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_plant_host_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, national_grid_operators).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, wright_law_learning_curve_decline).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, grid_flexibility_sufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build, own, and sell solar, wind, and battery projects. Tax credits, renewable portfolio standards, and green-finance channels steer large pools of capital toward their product line, and the trade associations they fund help define what counts as a serious climate plan in legislatures and boardrooms. Capital is globally mobile: a developer squeezed in one market redeploys pipelines to another.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers, agenda_setter).

% Run the campaigns, scorecards, and coalition letters through which climate credibility is assigned. Staff networks, donor bases, and public identity were formed in decades of anti-nuclear campaigning merged with renewables optimism; abandoning the renewables-first framing would mean telling members and funders that a core article of the movement's self-understanding was wrong. Funding follows campaign success, which the framing delivers.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations, beneficiary).

% Operates reactors and sells reactor technology under licensing regimes built for very long asset lives. Plants close early when procurement policy shifts to renewables-only channels, and new builds struggle to reach financial close once lenders read the technology as politically disfavored. Sunk capital, site-specific workforces, and safety-case obligations make redeployment slow and partial.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_energy_industry, payer,
    institutional, generational, constrained, global).

% Pay the bills that fund whichever buildout their jurisdiction chose. Where reactors retired before storage matured, tariffs absorbed gas purchases, transmission buildout, and capacity payments alongside the renewables themselves. There is no practical way to leave the grid, and bill attribution rarely reaches the pathway decision that set the costs.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers, payer,
    moderate, immediate, trapped, regional).

% Town economies organized around plant payrolls and property-tax payments. Closure announcements arrive as conclusions of state-level or corporate decisions made far away; the tax base and high-wage jobs leave, and relocating is costly for residents whose homes and skills are tied to the site.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_plant_host_communities, payer,
    powerless, biographical, constrained, local).

% Sell the fuel that fills firm-capacity gaps in grids that retired nuclear ahead of storage maturity. The benefit arrives through dispatch order and capacity payments rather than through authorship of the policy; global LNG markets let revenues shift to wherever scarcity pricing appears next.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_gas_operators, beneficiary,
    institutional, biographical, arbitrage, global).

% Carry statutory reliability obligations. Interconnection queues, curtailment management, ramping reserves, and capacity-adequacy assessments are daily work, and internal planning studies increasingly flag the cost of firm-capacity shortages. They publish the operational record that every side of the pathway argument cites.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, national_grid_operators, payer,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, national_grid_operators, observer).

% Spend the largest share of income on energy in their regions yet hold no seat in the proceedings where pathway choices are legitimated. Tariff pass-throughs reach them first; their objection to regressive cost allocation is voiced, if at all, secondhand through charities and ombudsman reports.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_poor_households, excluded,
    powerless, immediate, trapped, regional).

% Run capacity-expansion and production-cost models published in journals and agency reports. Several influential studies find that including firm low-carbon capacity lowers the total cost of deep decarbonization. They hold no product line, no membership rolls, and no fundraising dependence on any pathway winning.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, independent_energy_system_modelers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes dispersed capital, manufacturing capacity, and political will around a single deployable pathway: it gives investors a bankable signal, regulators a default procurement template, and the climate movement a shared actionable target, solving the mobilization collective-action problem by naming the cheapest, fastest lever available at scale.
% TRANSFER_FUNCTION: Moves capital — subsidies, tax expenditures, green-finance flows, R&D funding — from general taxpayer and ratepayer pools toward renewable generation and storage; moves legitimacy, attention, and career incentives away from nuclear engineering toward renewables; and moves operating nuclear assets toward early retirement.
% ABSENT_VOICES: Nuclear engineers, plant host communities, and energy-poor households had thin representation in the venues where the doctrine hardened — utility commissions, NGO strategy rooms, editorial boards, COP side-events. Ratepayers appear only as aggregate load. Developing-world energy-access advocates note the doctrine's capital-cycle assumptions fit wealthy grids with existing transmission far better than grids being built from scratch, and they were largely absent when the framing went global.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, capital allocation would reorganize within budget cycles: portfolio-pragmatist procurement would regain ground, nuclear life-extension and new-build proposals would re-enter financing conversations, advocacy coalitions would fracture and reform around technology neutrality, and subsidy architectures built on renewables-only eligibility would face immediate redesign. Deployment of renewables would continue — the underlying cost curves are real — but the sequencing, the closure schedule, and the legitimacy hierarchy would all rearrange.
% FOUNDING_PROBLEM: After decades of nuclear cost overruns, construction delays, and post-Chernobyl/Fukushima political retreat, climate mobilization needed a fast, cheap, scalable answer that could actually be financed and built; the doctrine supplied it by naming renewables-plus-storage as that answer and freeing the movement from a firm-power dependency it had learned to distrust.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — decarbonize fast and cheap — is corroborated from outside the benefiting parties by the IPCC scenario literature and IEA net-zero analyses, both of which center rapid renewables deployment in least-cost pathways. Corroboration of the PROBLEM is broad; corroboration of this reading's EXCLUSIVE answer is not: those same bodies include nuclear and firm low-carbon capacity in most deep-decarbonization scenarios, and grid-operator adequacy studies independently attest that the firm-power question remains open. No source outside the benefiting parties attests that renewables-plus-storage alone is settled as the fastest-cheapest complete pathway.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. Claimed type tangled_rope reflects the structure I believe true: a genuine coordination function (a single credible fastest-cheapest lever that mobilized trillions in deployment capital and solved the climate movement's collective-action problem) joined to asymmetric costs borne through the same structure (stranded nuclear assets, closure-devastated host towns, ratepayer tariffs inflated where nuclear exits preceded storage maturity), held in place by active enforcement (taxonomy exclusion, funding gatekeeping, scorecard politics, editorial dismissal). Metrics are descriptive: extractiveness 0.58 — substantial but bounded, because the doctrine's core deployment claim is largely true and its capital mobilization delivered real decarbonization; suppression 0.62 — enforcement is fiscal and discursive rather than physical, but it genuinely forecloses alternatives in the venues that matter; theater_ratio 0.31 — pledge summitry and net-zero pageantry are real but the underlying buildout is physical and growing; accessibility_collapse 0.50 — accepting the doctrine collapses nuclear-inclusive paths rhetorically while they persist factually (China, France, recent US and Japanese reversals); resistance 0.60 — sustained pro-nuclear environmentalism, portfolio-pragmatist institutions, and grid-operator planning documents keep the contest live. The measurement series runs on one shared seven-point grid (all three metrics authored at every point, t=0..30, mapping roughly 1995-2025); end-state values equal the base_properties scalars. Rising base_extractiveness across the interval will trip the T17 abductive trigger (mountain_extraction_accumulation) — appropriate here, since the doctrine accumulated rent-like overreach as its institutional power grew, though it never claimed natural-law status. Gain flow lands demonstrably on renewable_energy_developers: tax credits, mandates, and green-finance channels steer the material flows to that seat, which is why it is named rather than left diffuse. Fixing cost is prohibitive: for the agenda setters and legislators who could replace the doctrine with technology-neutrality, removal requires rupturing a fused advocacy coalition, writing down sunk campaign infrastructure, and reversing entangled subsidy law — costs exceeding the marginal benefit any single actor captures from fixing it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently, and the structural data explains why. From the advocacy seat, the doctrine is the movement's hard-won truth: the thing that finally made climate action bankable, defended against relapse into nuclear fantasy. From the nuclear-industry and host-community seats, the same structure operates as enforced foreclosure: plants die not because physics failed but because a legitimacy gate redirected capital and called the result inevitable. Ratepayers sit diffuse and trapped — they experience the doctrine only as bill line-items and cannot attribute them. The engine computes this per-seat divergence from power, exit, and declared position; the authored claim does not adjudicate it. Note also the coalition deficit: ratepayers and host communities share grievances but lack coalition infrastructure (dispersed, unorganized, no trade association), which is why measured resistance stays moderate despite broad latent objection — a standing possibility that organized ratepayer-host-community coalitions would raise resistance and shift computed classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Renewable_energy_developers sit nearest the beneficiary end (d low): the doctrine steers capital directly to them and their exit is arbitrage-grade (globally mobile project pipelines). Climate_advocacy_organizations derive low d as agenda-setting beneficiaries, but their identity_locked exit matters qualitatively — they cannot cheaply abandon the doctrine even if evidence turns, which amplifies their effective stake in its persistence. Fossil_gas_operators are beneficiaries with low d but incidental receipt: they collect through dispatch order, not authorship. Nuclear_energy_industry sits near the full-target end (d high): constrained exit via sunk, site-specific capital. Electricity_ratepayers and nuclear_plant_host_communities are trapped or constrained targets at high d — the classic profile where effective extraction is amplified by immobility. National_grid_operators are institutional targets with constrained exit, partially offset by observation-grade information. Energy_poor_households, as excluded voices, carry high latent d with zero agenda influence — the sharpest asymmetry in the story. Independent modelers are analytical: near-zero stake, full visibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — climate mobilization needed a fast, cheap, scalable lever after decades of nuclear cost overruns made the incumbent firm-clean option look like a dead end — remains live: decarbonization is unfinished and the fastest-cheapest-pathway question is empirically open. Mandatrophy is therefore NOT resolved, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges, which clears the zombie flag: the doctrine persists because its problem persists, not because its corpse is propped up. The classification earns its keep in both directions. Reading the doctrine as pure snare erases the genuine coordination achievement — the deployment acceleration is real, physical, and historically unprecedented, and a snare label would license discarding it. Reading it as pure rope erases the identifiable victims — the stranded assets, hollowed host towns, and regressive tariffs that the same structure produced. Tangled_rope holds both facts: coordination function and asymmetric extraction through one structure, actively enforced. The open question the omegas carry is whether the extraction share grows (gas capture, storage-cost plateau, identity-rigid enforcement) until the tangle resolves toward snare, or the coordination share grows (continued cost declines) until it relaxes toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (renewable_primacy) of the climate_mitigation_legitimacy kernel — which structural facts here are reading-relative rather than absolute?',
    'Generate the sibling-reading stories (baseload_necessity, portfolio_pragmatism, degrowth_sufficiency) over the same referent and compare victim sets, epsilon, and computed classification across readings.',
    'Under portfolio_pragmatism, nuclear exits the victim set and measured extraction falls sharply; under baseload_necessity, this doctrine reads as dangerous denial of a physical requirement. Classification is indexical to the reading seat, not a property of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed structure: victim set, epsilon, and type are properties of this reading, not of climate policy simpliciter.').

omega_variable(
    storage_cost_trajectory,
    'Will storage and firming-cost declines continue steeply enough to preserve the strong-form claim (renewables plus storage alone decarbonize fastest and cheapest) at deep penetration?',
    'Compare realized versus projected cost trajectories (NREL ATB, IEA WEO, LCOS series) through successive buildout vintages, and against system-cost results from capacity-expansion models at 80-100% clean shares.',
    'Continued decline strengthens the doctrine''s coordination function and pulls it rope-ward; a plateau validates the firm-power siblings and pushes the doctrine toward tangled-rope or snare territory as its gap-filling costs compound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_cost_trajectory, empirical, 'Whether the empirical foundation of the primacy claim survives at high renewable penetration.').

omega_variable(
    gas_gap_filling_benefit_status,
    'Is fossil gas''s benefit from the doctrine incidental sequencing, or evidence of structural capture — gas interests shaping renewables-primacy advocacy to defer nuclear competitors?',
    'Funding-flow and lobbying-disclosure analysis of advocacy coalitions advancing renewables-only positions, tracing gas-sector donations and shared personnel.',
    'Demonstrated capture would raise effective extraction substantially and push classification toward snare; purely incidental benefit keeps the tangled-rope reading intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gas_gap_filling_benefit_status, empirical, 'Whether the gas-seat benefit is parasitic drift or engineered.').

omega_variable(
    identity_lock_vs_evidence_response,
    'Is advocacy resistance to nuclear-inclusive pathways evidence-responsive or identity-fused (legacy anti-nuclear movement identity merged with renewables optimism)?',
    'Panel studies of advocacy-position updates following major new cost and reliability evidence (e.g., the post-2018 firm-power literature); measure position change, not stated openness.',
    'Identity fusion means the scalar suppression measure understates effective lock-in — the binding persists after structural barriers fall — and classification stability would depend on generational turnover rather than evidence arrival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_evidence_response, conceptual, 'Structural versus internalized mechanism sustaining the doctrine against counterevidence.').

omega_variable(
    closure_displacement_counterfactual,
    'When nuclear plants closed under renewables-primacy economics, what generation actually displaced them — renewables plus storage, or fossil fuels?',
    'Displacement econometrics on the German, Californian, and Japanese closure episodes: counterfactual dispatch modeling against observed emissions and fuel mix in the five years post-closure.',
    'Fossil displacement enlarges the victim set (ratepayers and the climate itself bear the gap) and raises epsilon; renewable displacement shrinks the harm attribution and supports the doctrine''s own accounting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_displacement_counterfactual, empirical, 'Size and composition of the harm the doctrine''s operation actually caused at closure sites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(renewable_primacy_reading_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(renewable_primacy_reading_tr_t0, observed).
narrative_ontology:measurement(renewable_primacy_reading_tr_t5, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(renewable_primacy_reading_tr_t5, observed).
narrative_ontology:measurement(renewable_primacy_reading_tr_t10, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(renewable_primacy_reading_tr_t10, observed).
narrative_ontology:measurement(renewable_primacy_reading_tr_t15, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(renewable_primacy_reading_tr_t15, observed).
narrative_ontology:measurement(renewable_primacy_reading_tr_t20, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(renewable_primacy_reading_tr_t20, observed).
narrative_ontology:measurement(renewable_primacy_reading_tr_t25, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(renewable_primacy_reading_tr_t25, observed).
narrative_ontology:measurement(renewable_primacy_reading_tr_t30, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(renewable_primacy_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(renewable_primacy_reading_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(renewable_primacy_reading_be_t0, observed).
narrative_ontology:measurement(renewable_primacy_reading_be_t5, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(renewable_primacy_reading_be_t5, observed).
narrative_ontology:measurement(renewable_primacy_reading_be_t10, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(renewable_primacy_reading_be_t10, observed).
narrative_ontology:measurement(renewable_primacy_reading_be_t15, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(renewable_primacy_reading_be_t15, observed).
narrative_ontology:measurement(renewable_primacy_reading_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(renewable_primacy_reading_be_t20, observed).
narrative_ontology:measurement(renewable_primacy_reading_be_t25, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(renewable_primacy_reading_be_t25, observed).
narrative_ontology:measurement(renewable_primacy_reading_be_t30, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(renewable_primacy_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(renewable_primacy_reading_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(renewable_primacy_reading_su_t0, observed).
narrative_ontology:measurement(renewable_primacy_reading_su_t5, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(renewable_primacy_reading_su_t5, observed).
narrative_ontology:measurement(renewable_primacy_reading_su_t10, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(renewable_primacy_reading_su_t10, observed).
narrative_ontology:measurement(renewable_primacy_reading_su_t15, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(renewable_primacy_reading_su_t15, observed).
narrative_ontology:measurement(renewable_primacy_reading_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(renewable_primacy_reading_su_t20, observed).
narrative_ontology:measurement(renewable_primacy_reading_su_t25, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(renewable_primacy_reading_su_t25, observed).
narrative_ontology:measurement(renewable_primacy_reading_su_t30, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(renewable_primacy_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the decarbonization pathway debate.' The natural-language concept covers at least four structurally distinct claims that cannot share one epsilon: renewables-primacy (this file), baseload-necessity, portfolio-pragmatism, and degrowth-sufficiency. They differ in victim sets (nuclear enters the victim set only under this reading and baseload-necessity's rival framing differs again), in empirical status (learning-curve cost data is robust; whole-system firm-power substitution is contested), and in enforcement profiles. Upstream/downstream: the learning-curve cost evidence upstream of this reading is cited as warrant against the baseload-necessity sibling, while grid-operator adequacy studies downstream feed back against this reading's strong form. All family members link via network.affects_constraints; none is evaluable via another's observables without violating epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
