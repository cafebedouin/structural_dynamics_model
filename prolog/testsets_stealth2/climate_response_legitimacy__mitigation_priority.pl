% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response Legitimacy Constraint
 *   domain: environmental/political/economic
 *
 * SUMMARY:
 *   The standing arrangement under contest is the global climate-policy
 *   paradigm that defines a legitimate response as emissions reduction
 *   achieved through technological innovation and carbon pricing, explicitly
 *   preserving economic growth and claiming to decouple it from emissions.
 *   The arrangement coordinates a genuine collective-action problem: climate
 *   change is a planetary externality no single actor can address, and price
 *   signals plus innovation pipelines are real coordination machinery. Riding
 *   on that machinery is asymmetric extraction: carbon-price incidence falls
 *   regressively on low-income energy households; gradualism preserves
 *   incumbent fossil reserve values that a rapid transition would strand;
 *   financial intermediaries collect fees on every layer of an increasingly
 *   complex offset and allowance architecture; and the arrangement's central
 *   wager — that decoupling will outpace cumulative emissions — is placed on
 *   behalf of future generations who cannot consent, exit, or negotiate. The
 *   claim/metric gap is deliberate: this reading presents the arrangement as
 *   near-pure coordination, while the authored metrics describe substantial
 *   extraction operating through the same structure; the engine measures that
 *   divergence rather than reconciling it. Sibling readings of the same
 *   kernel (adaptation_priority, degrowth_transformation) are separate
 *   constraint stories linked through the network section; they are not
 *   folded into this one.
 *
 * KEY AGENTS:
 *   - international_policy_bodies: Agenda-setter (institutional/arbitrage) — administers the treaty cycle, certifies offset methodologies, and defines what counts as a legitimate response
 *   - incumbent_fossil_producers: Primary beneficiary (powerful/arbitrage) — gradual, price-led transition defers demand destruction and keeps reserve assets bookable
 *   - clean_technology_sectors: Secondary beneficiary (organized/constrained) — receives subsidies and mandated demand; business model depends on the arrangement's continuation
 *   - carbon_market_financial_intermediaries: Beneficiary (powerful/arbitrage) — collects fees on allowances, offsets, and green finance regardless of atmospheric outcome
 *   - wealthy_nation_consumers: Beneficiary (organized/mobile) — consumption trajectories preserved while emissions are offshored through trade
 *   - low_income_energy_households: Payer (powerless/trapped) — bears the regressive share of carbon prices through fuel, heat, and electricity
 *   - frontline_climate_vulnerable_communities: Payer (powerless/trapped) — exposed to ongoing impacts while adaptation is subordinated in funding and agenda order
 *   - future_generations: Payer (powerless/trapped, universal scope) — inherit the downside of the decoupling wager without having been able to consent or exit
 *   - degrowth_movement: Excluded (moderate/mobile) — argues growth-dismantling is required; largely absent from negotiating venues and official scenarios
 *   - adaptation_first_constituencies: Excluded (moderate/constrained) — push resilience-funding parity; receive rhetorical acknowledgment and marginal budget lines
 *   - ipcc_assessment_bodies: Analytical observer (analytical/analytical) — assesses the physical problem and reports the ambition gap without adjudicating between readings of legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.5).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Response Legitimacy Constraint").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "environmental/political/economic").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '9c41fac7-0fcd-4942-be76-64c49a5d5846').
narrative_ontology:cs_kernel_codification('9c41fac7-0fcd-4942-be76-64c49a5d5846', formalized).
narrative_ontology:cs_authority_grounding('9c41fac7-0fcd-4942-be76-64c49a5d5846', expertise).
narrative_ontology:cs_interpretation_layer_present('9c41fac7-0fcd-4942-be76-64c49a5d5846').
narrative_ontology:cs_reading_relation('9c41fac7-0fcd-4942-be76-64c49a5d5846', climate_response_legitimacy__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('9c41fac7-0fcd-4942-be76-64c49a5d5846', climate_response_legitimacy__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('9c41fac7-0fcd-4942-be76-64c49a5d5846', foundational, growth_preservation_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(growth_preservation_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9c41fac7-0fcd-4942-be76-64c49a5d5846', growth_preservation_constitutes_legitimacy, instrumental).
narrative_ontology:cs_axiom('9c41fac7-0fcd-4942-be76-64c49a5d5846', secondary, carbon_pricing_innovation_sufficiency).
narrative_ontology:cs_axiom_status(carbon_pricing_innovation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('9c41fac7-0fcd-4942-be76-64c49a5d5846', carbon_pricing_innovation_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('9c41fac7-0fcd-4942-be76-64c49a5d5846', growth_compatible_mitigation_consensus).
narrative_ontology:cs_drift_state('9c41fac7-0fcd-4942-be76-64c49a5d5846', post_paris_global_stocktake, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9c41fac7-0fcd-4942-be76-64c49a5d5846', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_fossil_producers).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, clean_technology_sectors).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_market_financial_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, wealthy_nation_consumers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, low_income_energy_households).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, frontline_climate_vulnerable_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, wealthy_nation_consumers).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, green_growth_compatibility_thesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, environmental_kuznets_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, carbon_pricing_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene the treaty cycle, set nationally determined contribution expectations, certify offset methodologies, and administer compliance markets. They define what counts as a legitimate response and collect administrative authority, staffing, and agenda control from running the machinery. Their exit is arbitrage-grade: they wrote the rules and can reframe them without bearing the price signal themselves.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, international_policy_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate under a transition paced by prices and innovation rather than by production limits. Gradualism defers demand destruction and keeps reserve assets bookable for decades; participation in designing offset and capture schemes licenses continued output. They can hedge through diversification, relocation of production, and direct influence over policy design.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_fossil_producers, beneficiary,
    powerful, generational, arbitrage, global).

% Receive subsidies, tax credits, and guaranteed demand from renewable mandates and fleet standards, and genuinely deliver deployed capacity. Their business models are built on the continuation of the specific pricing-and-subsidy architecture, so they defend its design even where they press for higher ambition within it.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, clean_technology_sectors, beneficiary,
    organized, biographical, constrained, global).

% Broker allowances, offsets, verification services, and green bonds, collecting fees at every layer of an increasingly complex trading architecture. Revenue scales with transaction volume and instrument complexity rather than with atmospheric outcome, and they can reposition across jurisdictions and asset classes at will.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_market_financial_intermediaries, beneficiary,
    powerful, immediate, arbitrage, global).

% Maintain rising consumption while territorial emissions fall partly through imports embodying others' emissions. They pay carbon prices at the margin but their aggregate position is subsidized by offshored production and by a transition paced to avoid lifestyle disruption; consumption adjustment remains available to them in ways unavailable to poorer households.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, wealthy_nation_consumers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, wealthy_nation_consumers, payer).

% Bear the regressive share of carbon prices through heating, fuel, and electricity bills, with the least capacity to invest in efficiency upgrades, purchase electric vehicles, or absorb price volatility. Compensation schemes exist but are partial, administratively leaky, and politically fragile; leaving the energy system is not an option.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, low_income_energy_households, payer,
    powerless, immediate, trapped, national).

% Live with intensifying impacts — floods, heat, crop failure, sea-level rise — while adaptation and loss-and-damage mechanisms remain thin relative to exposure and subordinate to mitigation in both funding and agenda order. Their safest outcome depends on the rapid-decoupling bet being won, a bet they did not place and cannot influence from their position.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, frontline_climate_vulnerable_communities, payer,
    powerless, biographical, trapped, regional).

% Do not yet exist and therefore hold no seat, no vote, and no exit. They inherit whatever residual warming accumulates and whichever technological bets fail, including any shortfall between promised and delivered carbon removal. The arrangement's central wager — that decoupling will outpace cumulative emissions — is placed entirely on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Scholars, activists, and some municipal politicians who argue that wealthy nations must dismantle the growth imperative through working-time reduction, universal basic services, and democratic firm ownership. They publish in academic journals and organize outside official processes but are largely absent from negotiating venues, ministerial panels, and the scenario libraries that structure what futures officials consider thinkable.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, degrowth_movement, excluded,
    moderate, biographical, mobile, global).

% Representatives of impact-exposed regions and resilience-focused practitioners who press for adaptation funding parity with mitigation. They receive rhetorical acknowledgment and pilot-program budget lines, but the agenda ordering that puts emissions reduction first is not theirs to change, and their leverage is limited to moral suasion at summits.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, adaptation_first_constituencies, excluded,
    moderate, biographical, constrained, regional).

% Assess the physical science of climate change and the technical properties of response options, and produce the scenario frameworks that structure policy imagination. They document the persistent gap between pledged and required reductions but do not adjudicate between competing readings of what makes a response legitimate; their summaries require line-by-line governmental approval, which shapes what reaches policymakers.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, ipcc_assessment_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__mitigation_priority, incumbent_fossil_producers).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global externality-coordination problem: aligns dispersed sovereign and private actors on a single metric (emissions), creates comparable price signals for carbon across jurisdictions, and channels private capital toward low-carbon technology without requiring any actor to abandon growth or restructure ownership.
% TRANSFER_FUNCTION: Moves transition costs onto current energy consumers (regressively, via price incidence) and defers residual climate risk onto future generations, while moving subsidy flows, mandate-created demand, and market fee income toward technology firms and financial intermediaries; preserves existing growth trajectories and incumbent asset values throughout.
% ABSENT_VOICES: Future generations hold no seat anywhere in the arrangement and cannot object except through proxy advocacy. The degrowth movement and adaptation-first constituencies would contest the legitimacy criterion itself but are structurally outside the negotiating venues, official scenario exercises, and ministerial panels where the criterion is maintained. Frontline vulnerable communities hold thin formal representation relative to their exposure.
% DISAPPEARANCE_RATIONALE: If the growth-preserving mitigation paradigm vanished overnight, the entire architecture built on it — carbon markets, the NDC pledge-and-review cycle, net-zero accounting conventions, clean-tech investment pipelines sized to price signals — would lose its legitimating basis and reorganize around whichever rival reading captured the vacuum. The contest over what counts as legitimate climate response, currently deferred by this arrangement's dominance, would reopen immediately, and asset values priced on gradual transition would reprice violently.
% FOUNDING_PROBLEM: Climate change presented itself as a global collective-action failure: how to coordinate emissions reductions across sovereign actors with divergent interests, without halting the economic development that populations demand and that political survival requires.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated from outside the arrangement's benefiting parties: IPCC physical-science assessments, independent of any carbon-market or fossil interest, attest that the coordination problem exists and remains unsolved at required scale. However, no external body attests that the growth-preserving resolution specifically is the required one — that element is attested mainly by parties inside the arrangement (treaty bodies, green-growth institutions, market participants), while degrowth scholarship and climate-justice movements outside the beneficiary set actively dispute it. Stated plainly: the problem is externally corroborated; the growth-compatibility of the solution is not.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: substantial but not confiscatory, because the arrangement delivers real deployment of low-carbon technology alongside regressive cost incidence, rent capture in offset markets, and a deferred-risk bet whose downside lands on parties with no seat. Suppression is 0.50 and is primarily discursive-institutional rather than coercive: alternative readings are marginalized through venue control, scenario-library framing, and agenda ordering rather than prohibited — suppression here is a raw structural property and is deliberately not scaled by power or scope. Theater ratio is 0.45 and rising: net-zero pledges, voluntary offset retirement, and accounting-based compliance generate a growing performative layer atop real engineering activity. Accessibility collapse is low-moderate (0.40) because the sibling readings remain live and articulable — alternatives have not collapsed, they have been institutionally sidelined. Resistance is 0.60: fuel-tax protests, developing-country objections to carbon tariffs, climate-justice mobilization, and degrowth scholarship all contest the arrangement continuously. Coalition potential among the payer seats is structurally weak: low-income households and frontline communities are geographically and temporally split, and the largest creditor of the arrangement's wager — future generations — cannot organize at all, which is precisely why their entry into the victim set carries so much of the extraction load. The measurement series runs on one shared time grid (six points, all three metrics authored at every point) so temporal analysis reads aligned rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter position the arrangement is functioning coordination it built and administers: targets set, markets cleared, technology deployed. From the beneficiary positions it is a managed transition that rewards patience — asset values preserved, fee streams flowing, demand mandates arriving on schedule. From the trapped payer positions the same structure operates as extraction: prices paid regressively, adaptation deferred, and a wager placed on their behalf without consent. The sharpest divergence is temporal: every seated agent experiences the arrangement within a biographical or generational horizon, while the arrangement's largest liability matures on a civilizational horizon held by agents who do not yet exist. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the four beneficiary seats: fossil producers and financial intermediaries hold arbitrage-grade exit (they designed or can redesign their exposure), placing them nearest the beneficiary end; clean-technology sectors are beneficiaries with constrained exit — they gain from the arrangement but cannot easily reposition if its design changes. Victim declarations drive high directionality for the three payer seats: low-income households and frontline communities are trapped with powerless power atoms, sitting near the full-target end; future_generations are the limiting case — trapped, powerless, universal scope — the maximum-directionality seat the structure admits, since no exit conceptually applies. Wealthy-nation consumers sit nearer symmetric than the named beneficiaries: they pay carbon prices too, but their net position is subsidized by offshored emissions and preserved growth. Larger scopes amplify effective extraction at the payer seats (verification of deferred obligations is hardest exactly where the obligation lands), and the engine owns that arithmetic from the declared scope atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating emissions reductions across sovereign actors without halting development — is still live, so this is not a mandatrophy case and no resolved flag is declared. The classification discipline matters here in both directions: reading the arrangement as pure extraction (snare) would erase the genuine coordination achievement — carbon pricing does solve a real externality-coordination problem that nothing else has solved at scale — while reading it as pure coordination (rope) would excuse the regressive incidence, the rent capture in offset architectures, and the unconsented intergenerational wager. Tangled rope preserves both facts: coordination function and asymmetric extraction operating through the same enforcement machinery. The rising theater_ratio series is the early-warning indicator to watch: if the performative layer (pledges, offsets, accounting) continues displacing functional decarbonization, the arrangement drifts toward piton at the seats that cannot exit, even while its administrators continue collecting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_pace_adequacy,
    'Is absolute decoupling of economic output from territorial and consumption-based emissions achievable at the pace the arrangement''s legitimacy claim requires, or does the arrangement rest on an unproven empirical premise?',
    'Compare observed consumption-based decoupling rates in advanced economies against the rates implied by remaining carbon budgets; distinguish territorial accounting gains from emissions offshored through trade.',
    'If decoupling fails at required pace, the deferred intergenerational transfer becomes realized: future_generations move from contingent to realized victims and the arrangement''s effective extraction rises sharply; if it succeeds, much of the measured extraction was transition cost rather than rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_pace_adequacy, empirical, 'Whether the growth-preserving wager underlying the arrangement is empirically sound.').

omega_variable(
    cdr_moral_hazard_dependency,
    'Does the arrangement''s credibility depend on future carbon dioxide removal at scales never demonstrated, licensing current emissions against uncollateralized promises?',
    'Audit integrated assessment pathways relied upon in official target-setting for negative-emissions assumptions; compare modeled CDR deployment against observed delivery capacity year over year.',
    'If yes, a large share of permitted current emissions functions as extraction backed by promises the arrangement cannot honor, and the suppression of rapid-cut alternatives is partly explained as protection of that license.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_moral_hazard_dependency, empirical, 'Technological dependency of the arrangement on undemonstrated removal capacity.').

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the climate_response_legitimacy kernel; the sibling readings (adaptation_priority, degrowth_transformation) would restructure the beneficiary and victim sets entirely — degrowth_transformation names wealthy-nation current consumers as payers rather than beneficiaries, and adaptation_priority names frontline communities as primary intended beneficiaries. Where exactly do the readings disagree?',
    'Comparative analysis across the three linked stories: locate the disagreement in the legitimacy criterion itself (whether compatibility with economic growth is constitutive of a legitimate response, merely permissible, or irrelevant), not in the physical science all readings share.',
    'Cross-reading comparison changes which seats compute as targets and beneficiaries; this story''s victim set is conditional on the growth-preservation premise that its siblings deny or ignore.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates the mitigation_priority reading; sibling readings are separate constraints with different epsilon and different victim sets.').

omega_variable(
    regressive_incidence_correctability,
    'Is carbon pricing''s regressive incidence on low-income energy households intrinsic to price-led instruments, or an artifact of incomplete revenue recycling?',
    'Natural experiments across recycling designs: per-capita dividend schemes (Switzerland, Canada carbon rebates) versus general-revenue designs; measure post-transfer incidence by income decile.',
    'If correctable, part of the measured extraction at the household seat is implementation deficit rather than structural, and the tangled_rope reading holds at that seat; if intrinsic, the payer seats tilt the computed classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regressive_incidence_correctability, empirical, 'Whether the household cost burden is a fixable defect or a structural feature of price-led mitigation.').

omega_variable(
    suppression_mechanism_location,
    'Is the marginalization of alternative readings of climate legitimacy structural (control of negotiating venues, funding gatekeeping, agenda ordering) or internalized (policy professionals'' identities fused with the growth-compatible paradigm such that alternatives become unthinkable)?',
    'Track professional trajectories of analysts who exit the mainstream policy apparatus: if they retain the growth-compatibility frame after losing venue access, the suppression is substantially internalized.',
    'Internalized suppression persists even if venues open, meaning effective suppression exceeds the structural measure and would not fall with procedural reform alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_location, conceptual, 'Structural versus internalized mechanism behind the marginalization of sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 1997, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_mit_pri_tr_t1997, climate_response_legitimacy__mitigation_priority, theater_ratio, 1997, 0.2).
narrative_ontology:measurement_basis(clim_mit_pri_tr_t1997, observed).
narrative_ontology:measurement(clim_mit_pri_tr_t2003, climate_response_legitimacy__mitigation_priority, theater_ratio, 2003, 0.25).
narrative_ontology:measurement_basis(clim_mit_pri_tr_t2003, observed).
narrative_ontology:measurement(clim_mit_pri_tr_t2009, climate_response_legitimacy__mitigation_priority, theater_ratio, 2009, 0.3).
narrative_ontology:measurement_basis(clim_mit_pri_tr_t2009, observed).
narrative_ontology:measurement(clim_mit_pri_tr_t2015, climate_response_legitimacy__mitigation_priority, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(clim_mit_pri_tr_t2015, observed).
narrative_ontology:measurement(clim_mit_pri_tr_t2020, climate_response_legitimacy__mitigation_priority, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(clim_mit_pri_tr_t2020, observed).
narrative_ontology:measurement(clim_mit_pri_tr_t2025, climate_response_legitimacy__mitigation_priority, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(clim_mit_pri_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(clim_mit_pri_be_t1997, climate_response_legitimacy__mitigation_priority, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement_basis(clim_mit_pri_be_t1997, observed).
narrative_ontology:measurement(clim_mit_pri_be_t2003, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2003, 0.46).
narrative_ontology:measurement_basis(clim_mit_pri_be_t2003, observed).
narrative_ontology:measurement(clim_mit_pri_be_t2009, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2009, 0.5).
narrative_ontology:measurement_basis(clim_mit_pri_be_t2009, observed).
narrative_ontology:measurement(clim_mit_pri_be_t2015, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement_basis(clim_mit_pri_be_t2015, observed).
narrative_ontology:measurement(clim_mit_pri_be_t2020, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement_basis(clim_mit_pri_be_t2020, observed).
narrative_ontology:measurement(clim_mit_pri_be_t2025, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(clim_mit_pri_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_mit_pri_su_t1997, climate_response_legitimacy__mitigation_priority, suppression_requirement, 1997, 0.35).
narrative_ontology:measurement_basis(clim_mit_pri_su_t1997, observed).
narrative_ontology:measurement(clim_mit_pri_su_t2003, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement_basis(clim_mit_pri_su_t2003, observed).
narrative_ontology:measurement(clim_mit_pri_su_t2009, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2009, 0.42).
narrative_ontology:measurement_basis(clim_mit_pri_su_t2009, observed).
narrative_ontology:measurement(clim_mit_pri_su_t2015, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2015, 0.47).
narrative_ontology:measurement_basis(clim_mit_pri_su_t2015, observed).
narrative_ontology:measurement(clim_mit_pri_su_t2020, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2020, 0.49).
narrative_ontology:measurement_basis(clim_mit_pri_su_t2020, observed).
narrative_ontology:measurement(clim_mit_pri_su_t2025, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(clim_mit_pri_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate climate response' decomposes into three structurally distinct arrangements per the epsilon-invariance principle — mitigation_priority (this story), adaptation_priority, and degrowth_transformation. Each has its own epsilon, its own beneficiary/victim structure, and its own classification; forcing one story to cover all three would make epsilon observer-relative, which the chi formula forbids. Mitigation_priority is the currently dominant reading and sits upstream: it sets the resource-ordering and venue-access conditions under which the sibling readings operate (adaptation is funded residually; degrowth proposals are excluded from official scenario libraries), so its edges to both siblings are structural pressure edges, with the degrowth edge additionally a logical foreclosure. The upstream story's higher institutional entrenchment is exactly why its contested premises propagate into the siblings' operating environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
