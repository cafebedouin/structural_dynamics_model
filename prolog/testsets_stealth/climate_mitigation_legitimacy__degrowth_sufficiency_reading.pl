% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:directionality_override/3,
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
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   climate_mitigation_legitimacy: the degrowth-sufficiency claim that
 *   decarbonization requires demand reduction, making large-scale generation
 *   expansion unnecessary. Per the epsilon-invariance principle, the
 *   colloquial label 'how to decarbonize' decomposes into four structurally
 *   distinct constraint stories — baseload_necessity_reading,
 *   renewable_primacy_reading, portfolio_pragmatism_reading, and this one —
 *   each with its own epsilon, victim set, and classification, linked via
 *   network.affects_constraints. The epsilon referent here is the standing
 *   arrangement under contest: the degrowth-sufficiency claim as it actually
 *   operates in the legitimacy contest (discursive gatekeeping, funding
 *   allocation, planning obstruction, burden-sharing defaults), assessed with
 *   the reading's own evaluative frame for what counts as cost — NOT the
 *   steady-state society the reading endorses, which would render epsilon
 *   near zero by construction. The claim/metric gap is deliberate: the
 *   reading presents itself as pure coordination (a rope in its own
 *   self-description: shared sacrifice, mutual restraint), while the authored
 *   metrics describe a hybrid structure with a genuine coordination core and
 *   real asymmetric burdens — the engine measures that divergence rather than
 *   the author reconciling it.
 *
 * KEY AGENTS:
 *   - sufficiency_advocacy_networks: Primary beneficiary and agenda-setter (organized/identity_locked) — collects legitimacy, funding, and agenda control; fused with the framing professionally and ideologically
 *   - communities_opposing_generation_siting: Secondary beneficiary (moderate/constrained) — collects avoided local disruption under a public-interest shield
 *   - fossil_fuel_incumbents: Incidental beneficiary (institutional/arbitrage) — collects extended asset life opportunistically without defending the framing
 *   - nuclear_development_industry: Primary target (institutional/trapped) — entire pipeline delegitimized as growth-dependent
 *   - utility_scale_renewable_developers: Primary target (powerful/constrained) — growth model's premise denied even as its technology is praised
 *   - transmission_expansion_sector: Secondary target (organized/constrained) — pipeline shrinks under downsizing scenarios
 *   - global_south_energy_access_populations: Primary target (powerless/trapped) — ascent to abundance delegitimized before it begins
 *   - energy_intensive_growth_sectors: Target with partial exit (powerful/arbitrage) — demand growth reframed as the problem
 *   - southern_government_negotiators: Excluded voice (organized/constrained) — objection registered as self-interest
 *   - integrated_assessment_community: Analytical observer (analytical/analytical) — sees which assumptions each reading smuggles in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.56).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.5).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth-Sufficiency Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '888ad609-72b9-4f69-a081-fcf7c04ce45f').
narrative_ontology:cs_kernel_codification('888ad609-72b9-4f69-a081-fcf7c04ce45f', distributed).
narrative_ontology:cs_authority_grounding('888ad609-72b9-4f69-a081-fcf7c04ce45f', distributed).
narrative_ontology:cs_reading_relation('888ad609-72b9-4f69-a081-fcf7c04ce45f', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('888ad609-72b9-4f69-a081-fcf7c04ce45f', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('888ad609-72b9-4f69-a081-fcf7c04ce45f', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('888ad609-72b9-4f69-a081-fcf7c04ce45f', foundational, generation_expansion_unnecessary_given_demand_reduction).
narrative_ontology:cs_axiom_status(generation_expansion_unnecessary_given_demand_reduction, holdable).
narrative_ontology:cs_axiom_grounding('888ad609-72b9-4f69-a081-fcf7c04ce45f', generation_expansion_unnecessary_given_demand_reduction, empirically_contingent).
narrative_ontology:cs_axiom('888ad609-72b9-4f69-a081-fcf7c04ce45f', secondary, sufficiency_normatively_prior_to_supply_optimization).
narrative_ontology:cs_axiom_status(sufficiency_normatively_prior_to_supply_optimization, holdable).
narrative_ontology:cs_axiom_grounding('888ad609-72b9-4f69-a081-fcf7c04ce45f', sufficiency_normatively_prior_to_supply_optimization, deontological).
narrative_ontology:cs_reference_frame('888ad609-72b9-4f69-a081-fcf7c04ce45f', steady_state_within_planetary_boundaries).
narrative_ontology:cs_drift_state('888ad609-72b9-4f69-a081-fcf7c04ce45f', contemporary_post_ar6_energy_crisis, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('888ad609-72b9-4f69-a081-fcf7c04ce45f', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_networks).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, communities_opposing_generation_siting).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_development_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, transmission_expansion_sector).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_energy_access_populations).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_growth_sectors).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, steady_state_economics).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, rebound_effect_empirics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research institutes, NGOs, and movement organizations that elaborate and promote the demand-reduction framing: they publish scenario studies, advise municipal and EU-level policy processes, run sufficiency pilot programs, and staff the journals and conferences where the framing circulates. Grant income, citations, advisory seats, and career advancement flow to them when the framing is adopted. Leaving the position would mean abandoning the intellectual commitment their professional identities and curricula vitae are built on.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_networks, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_networks, agenda_setter).

% Rural and coastal communities confronting proposed wind farms, solar arrays, transmission corridors, or reactor sites. The demand-reduction framing hands them a public-interest argument that reaches beyond local nuisance: if the buildout is unnecessary, refusal protects landscapes rather than merely private comfort. They collect the avoidance of disruption, traffic, and landscape change; their leverage is procedural and local, exercised through planning hearings and permitting fights.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, communities_opposing_generation_siting, beneficiary,
    moderate, biographical, constrained, regional).

% Producers and generators of oil, gas, and coal. They do not advocate the demand-reduction framing and frequently attack it publicly, yet every policy cycle in which new clean generation is framed as unnecessary weakens buildout mandates and lengthens the economic life of existing fossil assets. Their gains are incidental and opportunistic; they hold diversified portfolios and can shift capital toward whichever jurisdiction ignores the framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).

% Reactor vendors, state nuclear programs, and utilities carrying nuclear construction pipelines. Their business case assumes decades of large-scale buildout; the demand-reduction framing declares the entire category unnecessary irrespective of delivered cost or carbon performance. Capital is sunk, workforces are technology-specific, licensing commitments span decades, and no comparable market exists in another sector, so withdrawal means writing off the enterprise.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_development_industry, payer,
    institutional, generational, trapped, continental).

% Developers and manufacturers of utility-scale wind, solar, and storage. The framing accepts their technology but denies the premise of their growth model: deployment capped at replacement level rather than expansion strands project pipelines and manufacturing capacity sized for continuous growth. They retain more technological and geographic flexibility than nuclear actors, but the market they built for is structurally smaller under this framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewable_developers, payer,
    powerful, biographical, constrained, global).

% Grid operators, transmission builders, and interconnection firms whose investment cases rest on connecting large volumes of new generation across long distances. Downsizing scenarios shrink their addressable pipeline; their assets are long-lived, geographically fixed, and cannot be repurposed to a demand-side business at scale.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, transmission_expansion_sector, payer,
    organized, biographical, constrained, continental).

% Populations across Africa, South Asia, and parts of Latin America awaiting first reliable electricity or the industrial energy base for development. Under a globally binding downsizing logic, their ascent to energy abundance loses legitimacy before it begins: finance and trade regimes shaped by the framing would steer them toward efficiency and sufficiency without ever passing through abundance. They hold no seat in the venues where the framing is elaborated and have no substitute pathway if it hardens into loan conditionality.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_energy_access_populations, payer,
    powerless, generational, trapped, global).

% Data-center operators, hydrogen producers, and electrifying heavy industry whose expansion plans presuppose abundant new clean supply. The framing labels their demand growth the problem to be designed away rather than served. They can relocate computing loads and plants toward jurisdictions that ignore the framing, which softens but does not remove their exposure, since supply chains and talent remain concentrated in framing-hostile regions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_growth_sectors, payer,
    powerful, immediate, arbitrage, global).

% Governments of developing countries negotiating climate finance, technology transfer, and carbon budgets. They would object that Northern-defined sufficiency freezes existing global inequality in place, but they negotiate inside assessment and treaty frameworks that treat aggregate demand trajectories as settled inputs, and their objection registers as self-interest rather than principle.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, southern_government_negotiators, excluded,
    organized, generational, constrained, global).

% Modelers and analysts running integrated assessment and energy-system models across all four readings of the mitigation-legitimacy contest. They price demand-reduction scenarios alongside generation-expansion scenarios and can see which assumptions each reading smuggles in as givens. They collect no rents from any outcome and publish under norms of scenario pluralism.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, integrated_assessment_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocacy_networks).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns expectations and behavior for collective demand reduction: because individual restraint is futile without broad participation, the framing supplies shared sufficiency norms, planning templates for non-expansion pathways, and a legitimacy standard that lets actors mutualize restraint instead of free-riding on one another's conservation.
% TRANSFER_FUNCTION: Moves legitimacy, grant funding, and policy attention away from generation-supply expansion toward demand-side transformation; moves the decarbonization burden from builders and investors onto consumers and aspirant economies; and defers material energy access for populations without it in order to preserve ecological headroom for populations that already have it.
% ABSENT_VOICES: Southern governments and energy-poor populations are structurally underrepresented in the Northern academic and NGO venues where the reading is elaborated; energy-sector workers facing foreclosed livelihoods have no seat in scenario workshops; future generations appear only through proxy advocates. Their absence is what lets the burden-sharing asymmetry pass as consensus.
% DISAPPEARANCE_RATIONALE: If the framing vanished overnight, municipal sufficiency programs would lose their organizing template, EU-level demand-side policy work would lose its intellectual sponsor, the advocacy field's funding and career structure would collapse, and the generation-expansion readings would face markedly less rhetorical friction in permitting and finance. The physical energy system would not rearrange, but the discursive and institutional arrangements built on the framing would.
% FOUNDING_PROBLEM: The recognition that aggregate energy and material demand grows faster than clean supply can be scaled, and that efficiency gains are consumed by rebound and growth, so mitigation strategies premised on indefinite supply-side expansion fail against both buildout-rate physics and ecological limits.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the advocacy set: the IPCC AR6 Working Group III demand-side chapters document mitigation potential from demand reduction comparable to supply-side measures; IEA efficiency analyses and the econometric rebound literature independently confirm that efficiency gains are partially consumed by demand growth. These sources attest the founding problem is live without endorsing the reading's stronger claim that expansion is unnecessary.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.56 at interval end) because the framing's burdens are real but unevenly borne: trapped seats (Southern populations, nuclear industry) bear compulsory-feeling costs while the advocacy seat collects legitimacy and funding, yet the coordination core — collective-action provision for demand reduction — is genuine and delivers real avoided-infrastructure and ecological value. Suppression is moderate (0.50) and structural-discursive rather than physical: funding gatekeeping, venue control, and planning obstruction, roughly 80% structural and 20% internalized (sufficiency identity makes exit unthinkable for the advocacy seat itself, but that lock binds the enforcer, not the targets). Theater is moderate-low (0.32): scenario studies and pilot programs are functional, but lifestyle-performative anti-growth signaling that reduces no emissions is a growing share of activity. Accessibility collapse is low (0.25): generation-expansion alternatives remain fully available and dominant — this reading collapses almost no one's options, it competes for legitimacy. Resistance is high (0.70): energy industries, consumer preferences, growth-dependent fiscal systems, Southern states, and labor all push back. The measurement series run on one shared time grid (2008, 2011, 2014, 2017, 2020, 2022, 2025) with all three metrics authored at every point; trajectories are monotonic, not cyclical — the reading gains institutional traction steadily rather than oscillating, so no intermittent-reinforcement mechanism is implicated.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical structural data. The advocacy seat experiences the framing as liberating truth-telling about limits; the nuclear seat experiences it as existential delegitimation indifferent to delivered performance; the renewables seat experiences it as a betrayal that praises the technology while denying its business model; the Southern-population seat experiences it as imposed austerity by those who already consumed their headroom; the fossil seat experiences it as weather — unwilled, exploitable, unowned. Same-level divergence is instructive: nuclear and renewables developers hold comparable nominal power (institutional vs powerful) but different exits (trapped vs constrained), because nuclear's specificity destroys its fallback options while renewables' modularity preserves partial mobility. Coalition potential exists for the powerless seat: Southern-state coalitions negotiating as blocs have converted exclusion into bargaining power before (equity debates in the UNFCCC), which is why the excluded-voice register matters.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the advocacy networks (primary collectors), siting communities (genuine avoiders), and fossil incumbents (declared but overridden — see below). Victim declarations drive high directionality for the two growth-dependent generation industries, the transmission sector, the Southern populations (highest: powerless, trapped, generational horizon), and energy-intensive sectors (damped by arbitrage exit). One override is authored: fossil_fuel_incumbents derive near-full-beneficiary directionality from their beneficiary declaration, but the derivation misreads their relationship — they do not defend the framing, gain only incidentally, and would abandon it instantly if buildout mandates hardened. Their true position is near-symmetric opportunism (d=0.45), and the override prevents the engine from counting them as constituency the framing subsidizes.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric mislabelings. Reading the framing as pure coordination (its self-description) erases the asymmetric burdens: the deferral of Southern access, the uncompensated expropriation of growth-dependent sectors, and the regressive domestic incidence of restraint norms. Reading it as pure extraction (the industry-counter-description: austerity ideology in green costume) erases the genuine coordination function: demand reduction is a real collective-action problem that no supply-side portfolio solves, and the framing provisionally solves it. The founding problem remains live (corroborated by IPCC AR6 WGIII and the rebound literature from outside the benefiting parties), so mandatrophy is NOT resolved — the arrangement has not outlived its function. The mismatch watch applies forward: if rebound empirics come back small, the founding problem dies while the advocacy apparatus persists, and the status-dead x world-rearranges combination flags capture/zombie dynamics for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (degrowth_sufficiency_reading) of the kernel climate_mitigation_legitimacy; sibling readings (baseload_necessity_reading, renewable_primacy_reading, portfolio_pragmatism_reading) instantiate different constraints with different victim sets — baseload makes renewables the victim, renewable_primacy makes nuclear the victim, portfolio makes neither — while this reading places BOTH nuclear and renewables in the victim set as growth-dependent. Where exactly is the disagreement located?',
    'Locate the structural fork: all three sibling readings condition their supply claims on given demand trajectories, while this reading contests the conditioning variable itself. Empirical tracking of which assumption each reading treats as revisable would resolve whether the contest is over supply technology or over demand-givenness.',
    'If the fork is demand-givenness, no sibling is logically foreclosed and all four readings coexist as live positions; if the fork is supply necessity, this reading''s core premise directly negates the siblings'' necessity claims and foreclosure relations emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel membership and the location of the four-way legitimacy contest.').

omega_variable(
    constraint_operation_scope,
    'Does the measured extraction profile reflect the reading''s current discursive operation (gatekeeping, funding, framing) or its potential operation if it gained binding state authority (quotas, rationing, finance conditionality)?',
    'Track instances where the reading acquires decision authority — EU sufficiency provisions, development-bank screening criteria, municipal binding targets — and re-measure extraction and suppression under authority versus under advocacy.',
    'Under binding authority the victim set''s costs become compulsory rather than atmospheric, and effective extraction for trapped seats (Southern populations, nuclear industry) rises sharply; the classification would migrate toward harder types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_operation_scope, conceptual, 'Whether the constraint is measured in its advocacy phase or its governance phase.').

omega_variable(
    incidental_fossil_benefit,
    'Is the fossil incumbents'' gain from the framing structural (the framing causally delays clean buildout) or coincidental (their assets would have run longer anyway for unrelated reasons)?',
    'Difference-in-differences on buildout mandates and fossil asset retirement schedules across jurisdictions with varying sufficiency-framing penetration, controlling for gas prices and permitting timelines.',
    'If structural, the framing functions partly as fossil-delay cover and its effective extraction rises above the authored base; if coincidental, the fossil beneficiary declaration is spurious and should be withdrawn, lowering measured asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_fossil_benefit, empirical, 'Whether the fossil-incumbent benefit is causal or coincidental.').

omega_variable(
    southern_deferral_intrinsic_or_artifact,
    'Is the deferral of Global South energy access an intrinsic feature of the reading''s burden logic, or an artifact of its Northern articulation that Southern-authored sufficiency formulations would remove?',
    'Survey Southern climate-justice movements and development economists for sufficiency formulations that pair Northern contraction with Southern expansion rights; test whether any published variant preserves the downsizing privilege while removing the access deferral.',
    'If intrinsic, the Southern population seat remains a victim and the asymmetry component of extraction stands; if artifact, a revised reading would drop that victim class and the constraint moves toward a cleaner coordination profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(southern_deferral_intrinsic_or_artifact, conceptual, 'Whether the equity defect belongs to the reading or to its authorship.').

omega_variable(
    rebound_effect_magnitude,
    'How large is economy-wide rebound, and can demand-side-only pathways hold emissions trajectories without supply expansion?',
    'Macroeconomic rebound estimates from panel studies and post-intervention energy-intensity data across countries with aggressive efficiency policy.',
    'Large rebound vindicates the reading''s necessity claim, raising its legitimacy and lowering resistance; small rebound undermines the necessity claim, pushing the framing toward theatrical maintenance of a solved problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebound_effect_magnitude, empirical, 'Empirical magnitude of rebound, the load-bearing fact beneath the necessity claim.').

omega_variable(
    enforcement_scaling_ceiling,
    'Can the reading''s enforcement scale beyond discursive gatekeeping (funding, venue control, planning obstruction) without acquiring coercive state instruments?',
    'Observe whether advocacy organizations seek or endorse binding instruments — mandatory sufficiency standards, consumption caps — as their discursive gains plateau.',
    'If enforcement stays discursive, suppression remains near the authored 0.5 ceiling; if it acquires state instruments, suppression ratchets upward and trapped seats experience the constraint qualitatively differently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_scaling_ceiling, empirical, 'Ceiling on enforcement modality and its consequence for the suppression profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 2008, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2008, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(clim_tr_t2011, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2011, 0.2).
narrative_ontology:measurement(clim_tr_t2014, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(clim_tr_t2017, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(clim_tr_t2022, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2022, 0.3).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(clim_be_t2008, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement(clim_be_t2011, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2011, 0.34).
narrative_ontology:measurement(clim_be_t2014, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement(clim_be_t2017, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2020, 0.47).
narrative_ontology:measurement(clim_be_t2022, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2022, 0.52).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2025, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2008, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2008, 0.32).
narrative_ontology:measurement(clim_su_t2011, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2011, 0.35).
narrative_ontology:measurement(clim_su_t2014, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2014, 0.38).
narrative_ontology:measurement(clim_su_t2017, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2017, 0.41).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(clim_su_t2022, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2022, 0.47).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2025, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, identity_coordination).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how to decarbonize' decomposes into four stories sharing the kernel climate_mitigation_legitimacy, linked via affects_constraints. Epsilon differs across the family because the victim sets differ: baseload_necessity_reading extracts from renewables developers; renewable_primacy_reading extracts from the nuclear industry; portfolio_pragmatism_reading extracts from neither technology camp (its extraction, if any, falls on swing taxpayers); this degrowth_sufficiency_reading extracts from BOTH generation camps plus Southern aspirant populations, because it denies the premise (growth) on which all their business cases rest. The upstream/downstream structure runs through assessment culture: IPCC demand-side findings feed this reading's legitimacy claims, while grid-reliability empirics feed the baseload reading's — the two evidence streams do not commingle, which is why the family members contaminate each other only through the shared kernel, not through shared evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
