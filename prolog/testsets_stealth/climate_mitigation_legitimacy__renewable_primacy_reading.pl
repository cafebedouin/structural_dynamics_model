% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable-Primacy Decarbonization Pathway Claim
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The claim that renewables plus storage can achieve full decarbonization
 *   faster and cheaper than nuclear operates as an institutionalized
 *   constraint on climate-policy discourse and capital allocation: it decides
 *   which technologies count as legitimate climate solutions, which projects
 *   clear green-finance screens, and which advocacy positions are respectable
 *   inside the coalition. It has a genuine coordination function — cost
 *   declines are real, deployment at scale is real, and a movement needs a
 *   legible program — and a real extraction function running through the same
 *   structure: nuclear capital and careers are starved, reactor developers
 *   lose financing channels, and the claim must be actively policed against
 *   integration-cost evidence and reliability warnings. KEY AGENTS (by
 *   structural relationship): renewable_energy_industry: Primary beneficiary
 *   (institutional/mobile) — collects subsidy access, mandates, and investor
 *   certainty; storage_technology_sector: Secondary beneficiary
 *   (organized/mobile) — assigned the balancing role, captures R&D and
 *   valuation upside; climate_advocacy_organizations: Agenda-setter and
 *   beneficiary (organized/identity_locked) — administers the claim's
 *   boundary and cannot abandon it without fracturing identity;
 *   nuclear_energy_sector: Primary target (organized/trapped) — bears
 *   finance-screen exclusion and forced early closures;
 *   advanced_reactor_developers: Secondary target (moderate/trapped) —
 *   starved of patient capital; electricity_ratepayers: Split seat
 *   (powerless/constrained) — cheap bulk energy where grids cope, integration
 *   costs where they strain; grid_reliability_engineers: Excluded voice
 *   (moderate/constrained); energy_scenario_modeling_community and
 *   intergovernmental_assessment_bodies: analytical observers. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   while the metrics describe moderately extractive, actively enforced
 *   operation — the engine computes per-seat types from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable-Primacy Decarbonization Pathway Claim").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '7304014d-80e3-49fa-a5b4-d1379bdad0b7').
narrative_ontology:cs_kernel_codification('7304014d-80e3-49fa-a5b4-d1379bdad0b7', distributed).
narrative_ontology:cs_authority_grounding('7304014d-80e3-49fa-a5b4-d1379bdad0b7', expertise).
narrative_ontology:cs_interpretation_layer_present('7304014d-80e3-49fa-a5b4-d1379bdad0b7').
narrative_ontology:cs_reading_relation('7304014d-80e3-49fa-a5b4-d1379bdad0b7', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('7304014d-80e3-49fa-a5b4-d1379bdad0b7', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('7304014d-80e3-49fa-a5b4-d1379bdad0b7', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('7304014d-80e3-49fa-a5b4-d1379bdad0b7', foundational, renewable_storage_sufficiency_for_full_decarbonization).
narrative_ontology:cs_axiom_status(renewable_storage_sufficiency_for_full_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('7304014d-80e3-49fa-a5b4-d1379bdad0b7', renewable_storage_sufficiency_for_full_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('7304014d-80e3-49fa-a5b4-d1379bdad0b7', foundational, nuclear_capital_cycle_opportunity_cost).
narrative_ontology:cs_axiom_status(nuclear_capital_cycle_opportunity_cost, holdable).
narrative_ontology:cs_axiom_grounding('7304014d-80e3-49fa-a5b4-d1379bdad0b7', nuclear_capital_cycle_opportunity_cost, empirically_contingent).
narrative_ontology:cs_axiom('7304014d-80e3-49fa-a5b4-d1379bdad0b7', secondary, distributed_generation_privilege).
narrative_ontology:cs_axiom_status(distributed_generation_privilege, holdable).
narrative_ontology:cs_axiom_grounding('7304014d-80e3-49fa-a5b4-d1379bdad0b7', distributed_generation_privilege, conventional).
narrative_ontology:cs_reference_frame('7304014d-80e3-49fa-a5b4-d1379bdad0b7', cost_curve_deployability_frame).
narrative_ontology:cs_drift_state('7304014d-80e3-49fa-a5b4-d1379bdad0b7', contemporary_post_cop28, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('7304014d-80e3-49fa-a5b4-d1379bdad0b7', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, storage_technology_sector).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_energy_sector).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, advanced_reactor_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, variable_renewable_cost_decline_trajectory).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, storage_learning_curve_economics).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, electrification_first_mitigation_strategy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufactures and deploys solar, wind, and grid-scale hardware. Receives investor certainty, tax-credit architectures, procurement mandates, and green-finance eligibility keyed to the primacy claim. Order books track the claim's policy wins; if the claim lost standing, the industry would still sell equipment into whatever framework prevails, so exit is a portfolio shift rather than an existential loss.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_industry, beneficiary,
    institutional, biographical, mobile, global).

% Supplies battery systems assigned the load-balancing and (aspirationally) seasonal-storage role in the primacy pathway. The claim drives valuation, venture capital, and R&D funding toward duration technology. Pivot options into electric-vehicle and consumer markets soften dependence on the claim's grid-policy expression.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, storage_technology_sector, beneficiary,
    organized, biographical, mobile, global).

% Run the campaigns, scorecards, and finance-screening pressure through which the claim is administered inside the climate coalition. Fundraising appeals, volunteer identity, and two generations of antinuclear positioning are fused with the primacy message; abandoning or even qualifying the claim reads internally as betrayal, so the organizations maintain it past the point where evidence alone would sustain it.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_organizations, beneficiary).

% Operates the existing fleet and sells plant services. Bears the claim's costs concretely: exclusion from green taxonomies and finance screens, advocacy opposition to license extension and new build, politically forced early closures. There is no pivot — the operators' assets, workforce skills, and regulatory licenses are the technology the claim marks as unnecessary.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_energy_sector, payer,
    organized, biographical, trapped, global).

% Startups and programs developing small modular and advanced reactors need patient capital and demonstration pathways spanning decades. The primacy claim frames their product category as a delay to decarbonization, starving them of institutional investors and policy support. Exit means writing off a decade of engineering; persistence means surviving on philanthropic and state capital.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, advanced_reactor_developers, payer,
    moderate, generational, trapped, global).

% Receive very cheap bulk energy where transmission is adequate and renewable output is strong, and carry system costs — backup capacity, grid reinforcement, curtailment, duration storage — wherever integration strains appear. They cannot choose their jurisdiction's resource mix; their bills record whichever bet their planners made under the claim's influence.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers, payer,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers, beneficiary).

% Plan and operate high-reliability grids and raise firm-capacity, inertia, and seasonal-storage concerns as penetration climbs. Inside coalition spaces their objections are frequently read as pro-nuclear advocacy rather than engineering, so many confine their warnings to technical venues the policy conversation does not reach.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_engineers, excluded,
    moderate, biographical, constrained, national).

% Produces the integrated-assessment and capacity-expansion scenarios policymakers cite. Publishes both renewable-dominant and portfolio cases; which pathway looks optimal turns on assumption choices — discount rates, learning rates, firm-capacity cost marks — that peer review polices for method far more vigorously than for conclusion.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_scenario_modeling_community, observer,
    institutional, generational, analytical, global).

% Synthesize the mitigation literature into assessment cycles and net-zero pathways. Their inclusion thresholds for technologies signal legitimacy to member governments. They have moved incrementally toward acknowledging firm low-carbon options in annexes and scenario databases while headline summaries continue to emphasize renewable deployment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, intergovernmental_assessment_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coalition mobilization problem: gives activists, investors, and policymakers a single legible, investable pathway — build solar, wind, and storage now — aligning supply chains, permitting priorities, and green-finance criteria around one deployment program instead of fragmenting effort across competing technology camps.
% TRANSFER_FUNCTION: Moves capital-allocation priority, subsidy access, and moral legitimacy from firm and dispatchable generation — above all nuclear — toward variable renewables and storage; moves careers, media attention, and institutional relevance inside the climate movement along the same gradient.
% ABSENT_VOICES: Nuclear host communities and plant workers facing closure have no seat in coalition governance; grid-reliability engineers raising firm-capacity concerns are treated as interested parties; Global South energy planners who need firm capacity per dollar are addressed with lectures rather than consultation. They sit outside the rooms where the pathway claim is ratified.
% DISAPPEARANCE_RATIONALE: Green taxonomies and finance screens would reopen the nuclear question within months; subsidy architecture and procurement mandates would rebalance toward firm low-carbon capacity; coalition messaging, donor appeals, and academic scenario defaults would all need renegotiation; nuclear vendors would regain agenda access they currently lack.
% FOUNDING_PROBLEM: In the late 2000s the climate movement needed an answer to 'what actually replaces fossil fuels' that was deployable immediately: nuclear was politically toxic after decades of antinuclear campaigning, capital-heavy, and slow to build, while solar and wind prices were falling fast enough to promise market-driven scale.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by UNEP Emissions Gap reporting (the mobilization problem persists — pledges still exceed delivery), IEA investment tracking (clean-energy capital flows concentrate in renewables), and independent grid-operator procurement records. Nuclear-sector parties corroborate that the mobilization problem is real while disputing the claim's exclusivity; no source outside the renewable-benefiting set attests the stronger exclusivity version.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness is 0.58: substantial but bounded, because the claim tracks genuine cost trajectories — this is not pure rent collection riding a fiction. Suppression is 0.62 and is a raw structural property, unscaled by power or scope: the enforcement is coalition boundary-policing, taxonomy exclusion, and funding gatekeeping rather than statute, but it is continuous and organized. Theater ratio is 0.36: real deployment sits underneath, while a growing share of activity is self-confirming scenario work whose assumptions encode the conclusion. Accessibility collapse is 0.42 — alternatives remain practicable and publishable (portfolio cases, nuclear builds proceeding in China and France, life extensions justified on reliability grounds), so the claim closes fewer exits than a snare would. Resistance is 0.60: sustained counter-advocacy, the COP28 nuclear-tripling pledge, and recurring reliability warnings show the claim must be defended, not merely stated. The measurement series run on one shared time grid (t=0,3,6,9,12,15) so every tracked metric is authored at every examined point; the monotonic rise models enforcement machinery maturing (taxonomy fights, finance screening) alongside accumulating extraction, not cyclical oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the nuclear payer seat the arrangement operates as enforced exclusion — locally snare-flavored, since the coordination story (cheap fast decarbonization) functions as cover for cutting the payer out of the legitimate-solutions set. From the renewable beneficiary seat the same structure is validation of engineering reality — locally rope-flavored. The advocacy agenda-setter seat experiences it as both: genuine mission and identity trap at once. Ratepayers straddle: beneficiaries of cheap bulk energy in well-sited grids, payers of integration costs in strained ones. The modeling community sees assumption-level nuance (firm-capacity cost marks, learning-rate choices) that is invisible from every advocacy seat, which is precisely why its observer position matters.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the renewable industry and storage sector toward the low-d (subsidized) end; their mobile exits dampen effective extraction further. Victims — nuclear sector and reactor developers — derive high d, amplified by trapped exits: neither can leave the technology the claim devalues. Climate advocacy organizations derive low d from their beneficiary position (they collect fundraising and relevance), and their identity_locked exit is recorded separately as an exit modulation rather than folded into d; no override is needed because the derivation from declarations plus exit options already captures each relationship. Ratepayers, carrying both payer and beneficiary roles, sit near symmetric. Observers take analytical treatment. Scope is global for the industry seats and regional for ratepayers, so the engine's scope amplification lands hardest on the diffuse payer seat — appropriate, since verification of system costs is hardest exactly there.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mobilizing deployment against political paralysis when nuclear was untouchable — is still live, so mandatrophy_resolved is not declared. The tangled_rope classification prevents two symmetrical mislabelings: calling this a snare erases the genuine coordination (cost declines, gigawatt-scale deployment, supply-chain learning that no extraction story explains); calling it a rope erases the asymmetric extraction (a victim set with trapped exits, enforcement that suppresses alternatives rather than merely coordinating them). The forward risk is piton drift: if deployment plateaus against integration limits while the scenario layer keeps assuming the conclusion, theater_ratio continues climbing and the claim's functional content atrophies into coalition ritual — the measurement series is designed to catch that transition early.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (renewable_primacy_reading) of the contested kernel climate_mitigation_legitimacy — what structural facts would differ if the same subject matter were instantiated under a sibling reading?',
    'Compile and compare the sibling stories: baseload_necessity_reading flips nuclear from victim to beneficiary and casts variable renewables as the constrained party; portfolio_pragmatism_reading dissolves the exclusive victim set into hedged allocation; degrowth_sufficiency_reading removes the generation-expansion premise entirely.',
    'Per-seat classifications, victim sets, and epsilon referents are reading-indexed; comparing epsilons across readings without this omega would misread divergent values as measurement disagreement rather than as different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a four-reading kernel; sibling deltas documented.').

omega_variable(
    sufficiency_disagreement_location,
    'Is the live disagreement between this reading and its siblings located in the sufficiency premise (renewables plus storage can reach full decarbonization alone) or in the comparative premise (faster and cheaper than nuclear)?',
    'Structural analysis of sibling axiom sets: baseload_necessity contests sufficiency outright; portfolio_pragmatism accepts partial sufficiency but contests exclusivity of the optimum. Locating the live disagreement determines which empirical program could resolve it — integration-cost studies versus comparative capital-cycle analysis.',
    'If the live disagreement is sufficiency, the constraint''s fate rides on deep-decarbonization tail evidence (seasonal storage, industrial heat, geographic variability); if comparative, it rides on nuclear capital-cycle performance (SMR delivery times and costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_disagreement_location, conceptual, 'Where the kernel contest is actually located for this reading.').

omega_variable(
    integration_cost_inflection,
    'Do system-level integration costs — transmission, curtailment, seasonal storage, firm backup — remain sub-linear as variable-renewable penetration exceeds roughly seventy percent, or inflect upward?',
    'Observed procurement and wholesale-market data from high-penetration grids (South Australia, California ISO, Ireland) as penetration climbs through the seventies and eighties.',
    'Sub-linear costs support the coordination component and cap effective extraction; an inflection would mean the claim is increasingly maintained against evidence, raising suppression and pushing computed seats toward snare-flavored classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_cost_inflection, empirical, 'Whether the claim''s empirical foundation strengthens or erodes at high penetration.').

omega_variable(
    advocacy_identity_lock_share,
    'How much of the claim''s persistence among advocacy organizations tracks current evidence versus inherited antinuclear identity formed before the storage-cost revolution?',
    'Counterfactual update test: observe coalition response to a credible small-reactor cost-and-schedule success; rapid updating indicates evidence-tracking, boundary-policing indicates identity lock.',
    'A high identity share raises the internalized component of suppression — the coalition carries the constraint with it even where external barriers fall — and strengthens the reading that enforcement serves the coalition''s self-concept rather than mitigation outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocacy_identity_lock_share, conceptual, 'Evidence-tracking versus identity-maintenance share in the enforcing seat.').

omega_variable(
    ratepayer_victim_status,
    'Are electricity ratepayers genuinely victims of the primacy arrangement, or beneficiaries whose apparent costs are transitional integration investments?',
    'Jurisdictional natural experiment comparing total-system cost per delivered low-carbon kilowatt-hour across portfolios with and without firm low-carbon capacity at comparable penetration.',
    'If ratepayers are net beneficiaries, the victim set narrows to nuclear capital and labor and the extraction asymmetry concentrates sharply; if net payers, diffuse harm widens the target set and scope amplification raises effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ratepayer_victim_status, empirical, 'Contested membership of the diffuse payer seat in the victim set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t3, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(clim_tr_t9, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 15, 0.36).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t3, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(clim_be_t9, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 9, 0.5).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clim_su_t3, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(clim_su_t9, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 9, 0.56).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how should we decarbonize' covers four structurally distinct claims with different beneficiary/victim sets, different coordination functions, and different epsilon values — decomposed per the epsilon-invariance principle into four linked stories sharing the kernel climate_mitigation_legitimacy. This reading (renewable_primacy) is upstream of portfolio_pragmatism in discourse: its dominance changes the legitimacy conditions under which technology-neutral positions are received, without resolving the dispute. It stands in logical contrariety with baseload_necessity on the sufficiency question. Each member links to the others via affects_constraints; orphaning any member would hide the family structure that explains why identical evidence produces divergent classifications across the set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
