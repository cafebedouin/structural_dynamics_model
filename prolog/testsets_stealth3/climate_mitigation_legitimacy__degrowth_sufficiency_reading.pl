% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth-Sufficiency Gate on Mitigation Legitimacy
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   Within the climate_mitigation_legitimacy kernel, this file instantiates
 *   one reading only: the degrowth-sufficiency claim that legitimate
 *   decarbonization runs through demand reduction, rendering large-scale
 *   generation expansion unnecessary. As an operative arrangement — in
 *   movement fora, aligned parties, and municipalities where the reading
 *   holds sway — the claim gates legitimacy: supply-side proposals (reactors,
 *   transmission corridors, utility-scale renewables) are framed as category
 *   errors rather than engineering choices, while efficiency, retrofit, and
 *   curtailment monopolize the serious-options register. The claim/metric gap
 *   is deliberate: the reading is CLAIMED here as tangled_rope (there is a
 *   genuine coordination core — demand-side measures are repeatedly the
 *   cheapest abatement, and restraint solves real collective-action problems)
 *   while the authored metrics describe substantial, actively enforced
 *   extraction: growth-dependent energy sectors bear closure costs, advocacy
 *   seats collect agenda control, and the least-shielded households absorb
 *   regressive burdens. Per the epsilon-invariance decomposition rule, the
 *   sibling readings are separate constraint files with their own epsilon
 *   values and victim sets, linked through the network block; this file
 *   neither averages over nor adjudicates them.
 *
 * KEY AGENTS:
 *   - sufficiency_advocacy_organizations: agenda-setter and collector (organized/identity_locked) — administers the doctrine and collects agenda control
 *   - degrowth_research_networks: beneficiary (moderate/identity_locked) — collects scholarly capital and relevance
 *   - host_community_anti_siting_coalitions: incidental beneficiary (moderate/mobile) — collects avoided siting burdens
 *   - nuclear_vendors_operators: primary target (institutional/trapped) — bears market closure
 *   - renewable_developers: primary target (powerful/constrained) — bears pipeline delegitimization
 *   - transmission_grid_builders: secondary target (institutional/constrained) — bears stranded order books
 *   - energy_intensive_industries: target with partial arbitrage (powerful/arbitrage) — bears output ceilings, can relocate
 *   - generation_construction_workforce: target (organized/constrained) — bears stranded skills and pensions
 *   - energy_poor_households: least-shielded target (powerless/trapped) — bears regressive demand cuts
 *   - household_energy_consumers: dual-positioned (moderate/constrained) — bears prescriptions, gains avoided costs
 *   - global_south_development_planners: excluded voice (organized/constrained) — objects from outside the drafting rooms
 *   - climate_policy_assessment_bodies: analytical observer (institutional/analytical) — registers the empirical contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.64).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth-Sufficiency Gate on Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '192f7d73-f6c6-4b5c-ac17-09645dcd0077').
narrative_ontology:cs_kernel_codification('192f7d73-f6c6-4b5c-ac17-09645dcd0077', distributed).
narrative_ontology:cs_authority_grounding('192f7d73-f6c6-4b5c-ac17-09645dcd0077', distributed).
narrative_ontology:cs_reading_relation('192f7d73-f6c6-4b5c-ac17-09645dcd0077', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('192f7d73-f6c6-4b5c-ac17-09645dcd0077', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('192f7d73-f6c6-4b5c-ac17-09645dcd0077', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('192f7d73-f6c6-4b5c-ac17-09645dcd0077', foundational, demand_reduction_suffices_without_expansion).
narrative_ontology:cs_axiom_status(demand_reduction_suffices_without_expansion, holdable).
narrative_ontology:cs_axiom_grounding('192f7d73-f6c6-4b5c-ac17-09645dcd0077', demand_reduction_suffices_without_expansion, empirically_contingent).
narrative_ontology:cs_axiom('192f7d73-f6c6-4b5c-ac17-09645dcd0077', foundational, affluent_demand_reduction_is_justice_imperative).
narrative_ontology:cs_axiom_status(affluent_demand_reduction_is_justice_imperative, holdable).
narrative_ontology:cs_axiom_grounding('192f7d73-f6c6-4b5c-ac17-09645dcd0077', affluent_demand_reduction_is_justice_imperative, deontological).
narrative_ontology:cs_reference_frame('192f7d73-f6c6-4b5c-ac17-09645dcd0077', sufficiency_governed_mitigation_order).
narrative_ontology:cs_drift_state('192f7d73-f6c6-4b5c-ac17-09645dcd0077', contemporary_buildout_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('192f7d73-f6c6-4b5c-ac17-09645dcd0077', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_organizations).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_research_networks).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, host_community_anti_siting_coalitions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_vendors_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, transmission_grid_builders).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, generation_construction_workforce).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, household_energy_consumers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, household_energy_consumers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_first_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, decoupling_skepticism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the campaigns, publish the roadmaps, and police the boundaries of what counts as serious climate policy inside movement fora, aligned parties, and sympathetic municipalities. Adoption of the demand-reduction-first doctrine hands them agenda control, membership growth, and funder salience; enforcing it against supply-side proposals is their daily operational work. Leaving the position would mean repudiating the founding commitments their staff, donors, and members organize around.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_organizations, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_organizations, beneficiary).

% Produce the conference circuits, journals, and teaching programs through which the sufficiency framing circulates. Careers, citation networks, and grant lines are built on the framework's premises; pivoting to supply-side research questions would strand accumulated scholarly capital and sever professional relationships.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_research_networks, beneficiary,
    moderate, biographical, identity_locked, continental).

% Organize residents opposing wind farms, transmission corridors, and reactor siting in their locales. The downsizing-first priority validates and amplifies their objections, sparing them infrastructure burdens without requiring them to articulate a national energy strategy. Their stake ends at the county line; they can disband once a project is defeated and re-form for the next one.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, host_community_anti_siting_coalitions, beneficiary,
    moderate, immediate, mobile, regional).

% Design, sell, and operate reactors whose economics require fleet buildout and long-term purchase commitments. A mitigation regime that declares large-scale generation expansion unnecessary closes their addressable market regardless of their safety or cost performance; their capital, licenses, and trained workforce are sunk into exactly the activity the doctrine delegitimizes, and none of it pivots to retrofit work.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_vendors_operators, payer,
    institutional, generational, trapped, global).

% Develop solar, wind, and storage projects on the premise that decarbonization means electrifying everything with clean supply. The doctrine reframes their product line as unnecessary expansion, threatening pipelines, offtake contracts, and investor theses. They can shift geography toward friendlier jurisdictions but cannot exit generation development as an industry.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_developers, payer,
    powerful, biographical, constrained, global).

% Plan and build the wires that connect new generation to load. Minimal-new-capital-deployment policy strands approved corridor projects, freezes hiring, and empties planning queues; their entire order book consists of the expansion activity the doctrine rules out.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, transmission_grid_builders, payer,
    institutional, generational, constrained, continental).

% Operate steel, cement, aluminum, and chemicals plants whose output scales with energy input. Demand caps and sufficiency instruments raise compliance costs and ceiling their output; they retain partial escape by relocating capacity to jurisdictions without such limits, at the price of stranding domestic assets and workforces.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries, payer,
    powerful, biographical, arbitrage, global).

% Unionized trades whose employment pipeline is plant and grid construction. Downsizing-first policy converts their apprenticeship systems and pension contribution streams into stranded obligations; their skills do not transfer cleanly to retrofit and efficiency work at equivalent wages, and retraining promises have historically underdelivered.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, generation_construction_workforce, payer,
    organized, biographical, constrained, national).

% Already under-consume energy relative to need: cold homes, deferred appliance replacement, forgone travel. Demand-reduction mandates and price-based sufficiency instruments ask further cuts from households with the least slack, converting a mitigation instrument into a regressive burden they have little capacity to refuse, offset, or relocate away from.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_households, payer,
    powerless, immediate, trapped, national).

% Face lifestyle prescriptions, curtailment requests, and mandated efficiency retrofits as the doctrine operationalizes demand reduction. They also stand to gain from avoided generation costs showing up in bills and from cleaner air if the strategy delivers; their net position depends on whether the savings reach tariffs or are absorbed elsewhere in the system.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, household_energy_consumers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, household_energy_consumers, beneficiary).

% Plan electrification and industrialization for populations whose per-capita energy use is a fraction of affluent-world levels. Universally framed demand-reduction doctrine treats their growth as the problem to be capped; they hold that sufficiency applied globally entrenches poverty, but they hold few seats in the movement fora, foundation panels, and municipal consultations where the doctrine's text is actually drafted.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_development_planners, excluded,
    organized, generational, constrained, continental).

% Run the integrated models and country reviews through which mitigation strategies are compared. They register that demand-side measures are consistently cheap early abatement while nearly all deep-decarbonization scenarios nonetheless assume very large clean-supply buildout, and they watch the widening gap between the doctrine's claim and the modeled record without adjudicating it.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_policy_assessment_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_organizations).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates aggregate restraint: because emissions scale with energy demand, getting many actors to consume less — through efficiency standards, retrofit programs, sufficiency norms, and curtailment — avoids the need to finance, site, and integrate equivalent new generation, and dissolves the siting-conflict and material-throughput problems that expansion multiplies.
% TRANSFER_FUNCTION: Moves legitimacy, capital, and political attention away from generation-expanding sectors toward demand-side institutions: market closure and pipeline delegitimization fall on nuclear vendors, renewable developers, grid builders, and construction trades; agenda control, funder salience, and moral authority accrue to sufficiency advocacy and research seats; behavioral and price burdens fall on households, disproportionately on those with the least consumption slack.
% ABSENT_VOICES: Global South development planners would object that universally applied demand-reduction framing caps legitimate development energy growth; energy-poor households would object that further cuts are demanded of those with the least slack; expansion-dependent workers would object to the ease with which their trades are written off. They are absent from the movement fora and consultation processes where the doctrine is elaborated; their objections surface in UNFCCC equity negotiations and union submissions, outside the rooms where the text is drafted.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, blocked generation projects would re-enter permitting, capital parked behind sufficiency theses would rotate back to supply-side theses, advocacy coalitions would lose their gating position and part of their membership rationale, and mitigation discourse would rebalance toward the portfolio and renewable-primacy frames that currently operate in its shadow.
% FOUNDING_PROBLEM: In the 1970s energy crises, analysts observed that every increment of clean supply was chased by demand growth, leaving emissions and fossil dependence intact while capital and decades were consumed; the soft-energy-path argument held that investing in demand-side efficiency and sufficiency outcompetes supply expansion on cost and speed. The arrangement was built to solve mitigation that stalls because demand growth outruns deployment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: IEA and IRENA efficiency analyses repeatedly rank demand-side measures among the cheapest abatement options, and IPCC mitigation chapters document large unexploited demand-side potential — attesting that the founding problem is real. The same sources, however, dispute the strong form of this reading: their deep-decarbonization scenarios pair demand measures with very large clean-supply buildout, so external corroboration extends to the problem's liveness, not to the claim that expansion is unnecessary.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (0.64) reflects a real transfer: the doctrine converts other actors' business models and career pipelines into illegitimacy while its own seats collect agenda control, and it operates at continental-to-global scope where verification that 'sufficiency was delivered' is weakest. Suppression (0.58) is discourse-grade rather than statutory — heresy-policing, funder gatekeeping, moral sanction of techno-optimism — but within adopting institutions alternatives collapse markedly (accessibility_collapse 0.55) while the wider policy field keeps them live. Resistance (0.70) is high because every producer seat, the construction trades, and development planners contest the doctrine openly. Theater (0.42) tracks the widening gap between advocacy and practice: manifestos, conferences, and sufficiency pledges multiply faster than measured absolute demand falls, and advocating restraint is cheaper than practicing it. Receipt surface: the doctrine's yield — agenda control, funder salience, blocked rivals — demonstrably accrues to the advocacy seat, so gain_flow names it rather than asserting diffuse. Cost to fix: the seats positioned to relax the doctrine (advocacy leadership, aligned funders) would pay in identity repudiation and coalition rupture, so fixing_cost is prohibitive from where they stand. The measurement series run on one shared grid (t=0..30, six points, all three metrics at every point); trajectories are monotonic with no cycle, and enforcement hardens as counterevidence accumulates.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the advocacy seat the arrangement is overdue correction: it stops waste and names the real driver of emissions. From the vendor, developer, grid-builder, and workforce seats it is market closure administered as morality. From energy-poor households it is austerity wearing mitigation clothing. From Global South planners it is a ceiling on development exported from affluent economies that got rich on expansion. Assessment bodies see the empirical contest registered but unresolved. The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the research and anti-siting seats; the advocacy seat's identity lock pushes it deeper into subsidy-of-the-frame than its formal collecting role alone would. Victim declarations drive high d for the growth-dependent producers: trapped exit (vendors) and skill lock (workforce) amplify, while industrial arbitrage damps. Energy-poor households sit nearest full-target — least slack, no exit. Household consumers are dual-declared and land near symmetric: the payer role raises d, incidental bill benefits lower it. No directionality override is authored: the moderate power atom is shared between beneficiary seats whose d must stay low and the dual-role household seat, so a single atom-level override could not differentiate them, and the derivation from declared roles plus exit options already lands each seat correctly. Excluded voices (Global South planners) are commentary-grade only and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — demand growth outrunning clean-supply deployment — is corroborated as live by sources outside the beneficiary set (IEA/IRENA efficiency rankings, IPCC demand-side chapters), so the arrangement is not yet a piton: its function has not atrophied, and no sunset clause is declared. Classification as tangled_rope prevents two opposite mislabels: reading the doctrine as pure coordination (which would hide the closure costs borne by named producer seats and the regressive incidence on poor households) and reading it as pure extraction (which would erase the genuine coordination core that every independent assessment confirms). The theater trajectory is the early-warning channel: if advocacy-to-practice conversion keeps falling while enforcement hardens, the arrangement drifts toward performing sufficiency rather than delivering it, and the founding-problem-status x disappearance-verdict mismatch consumer would flag capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the degrowth_sufficiency_reading of the climate_mitigation_legitimacy kernel; how would the constraint''s structure change under each sibling reading?',
    'Author the sibling files: baseload_necessity_reading restores dispatchable suppliers to the beneficiary position and shifts victims toward variable-renewable critics; renewable_primacy_reading moves nuclear to the victim set and renewables to beneficiaries; portfolio_pragmatism_reading dissolves the single-victim structure into technology-neutral allocation.',
    'Victim/beneficiary sets, epsilon, and per-seat classifications differ per reading; cross-reading comparison is valid only file-by-file, never averaged over the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one reading of a four-reading kernel; disagreement located at whether large-scale generation expansion is necessary.').

omega_variable(
    sufficiency_empirical_adequacy,
    'Can demand reduction and efficiency realistically substitute for large-scale generation expansion under full electrification of transport, heating, and industry?',
    'Integrated assessment model ensembles, historical income-elasticity of electricity demand in developing economies, and post-program rebound studies of efficiency interventions.',
    'If substitution fails, the coordination story collapses toward pure expansion-blocking (the victim set widens, epsilon rises); if it holds, measured extraction is overstated and the reading approaches genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_empirical_adequacy, empirical, 'Whether the reading''s central empirical premise survives contact with electrification-driven demand growth.').

omega_variable(
    agenda_capture_vs_diffuse_gain,
    'Does the doctrine''s yield — agenda control, funding salience, blocked rivals — accrue to the advocacy seat, or dissipate across diffuse publics?',
    'Movement financing records, staffing trajectories at aligned foundations, and traced provenance of the advocacy that defeated specific generation projects.',
    'Named-seat capture supports the tangled-rope reading with a snare-leaning refinement; genuinely diffuse gain would soften the extraction asymmetry and weaken the capture cell of the receipt surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agenda_capture_vs_diffuse_gain, empirical, 'Whether gains concentrate in the advocacy seat or remain diffuse.').

omega_variable(
    advocate_identity_lock_depth,
    'How deep is the identity lock on advocacy and research seats — would public recantation actually cost careers, funding, and membership, or is exit cheaper than modeled?',
    'Track the standing and funding of prominent figures after public shifts toward supply-side positions; ecomodernist defections serve as a natural experiment.',
    'Shallow lock raises beneficiary-seat mobility, damping their directional subsidy and tightening the extraction asymmetry; deep lock sustains the current structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocate_identity_lock_depth, empirical, 'Depth of identity fusion binding the doctrine''s administrator and research seats.').

omega_variable(
    biophysical_necessity_framing,
    'Is the sufficiency requirement what the reading''s rhetoric presents — a biophysical necessity akin to natural law — or a constructed policy preference maintained by coalition?',
    'Test the claim against the planetary-boundaries literature the reading cites: the science bounds total throughput but does not dictate demand-reduction-first over supply-transformation pathways.',
    'If treated as constructed, the arrangement forfeits any mountain-like immunity and stands or falls on coalition maintenance; the naturality framing is precisely what shields it from revision pressure, so resolving this omega changes how much enforcement its persistence actually requires.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biophysical_necessity_framing, conceptual, 'Natural-law versus constructed status of the sufficiency requirement as the reading presents it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement_basis(clim_tr_t18, observed).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(clim_tr_t24, observed).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(clim_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement_basis(clim_be_t18, observed).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(clim_be_t24, observed).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement_basis(clim_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement_basis(clim_su_t18, observed).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(clim_su_t24, observed).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(clim_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how to decarbonize' decomposes into four structurally distinct readings of the climate_mitigation_legitimacy kernel, each with its own epsilon, victim set, and classification. This file is the degrowth_sufficiency_reading only: uniquely among the four, it places BOTH nuclear and renewables in the victim set as growth-dependent, privileges system downsizing, and minimizes new capital deployment. The upstream empirical record (assessment-body scenario literature) influences all four readings but settles none of them; the siblings are separate files linked here per the epsilon-invariance rule — measuring this constraint through a sibling's observable would change epsilon, which is the signature that they are different constraints, not one constraint viewed from angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
