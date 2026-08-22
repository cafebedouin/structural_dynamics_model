% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable-Risk Standard for Energy Pathways
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This story instantiates the expected_value_dominant reading of the
 *   acceptable_risk_energy kernel: the claim that an energy pathway's risk is
 *   acceptable when aggregate expected harm, commensurated as mortality per
 *   terawatt-hour, is minimized across all available pathways. The standing
 *   arrangement under contest is the actual operation of this standard in
 *   contemporary energy governance — regulatory impact analysis, comparative
 *   risk assessment, carbon-pricing justification, and investment screening —
 *   not the idealized decision procedure its proponents describe. Assessed by
 *   the reading's own lights (its own arithmetic taken seriously), the
 *   standard's actual operation delivers real harm reduction where it binds —
 *   coal retirements justified by body counts, financing exclusion of the
 *   highest-mortality fuels — while chronically failing its own test: the
 *   largest energy mortality toll (combustion air pollution, mining)
 *   continues at scale because the standard's suppression of the fossil
 *   pathway is politically defeasible, and its gains accrue
 *   disproportionately to positioned actors (low-carbon capital, the nuclear
 *   industry, the analytic-administrative class whose offices depend on the
 *   framework) while its concentrated costs land uncompensated on trapped
 *   fossil-dependent communities. FAMILY DECOMPOSITION NOTE
 *   (epsilon-invariance): the colloquial label 'acceptable energy risk'
 *   covers three structurally distinct criteria that share one referent — the
 *   operative acceptable-risk regime — and author different epsilon values
 *   from it. This file authors the expected_value_dominant reading alone,
 *   with a single stable epsilon (0.64): fossil-pathway deaths enter the
 *   victim set at full weight, nuclear accidents are probability-discounted
 *   to near-invisibility, and suppression concentrates on the fossil pathway.
 *   The catastrophic_tail_dominant sibling would weight nuclear-accident
 *   victims at full dread magnitude and target suppression at the nuclear
 *   pathway; the option_value_preserving sibling would count foreclosed
 *   pathway flexibility as the primary harm. Separate files, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - fossil_fuel_incumbents: Primary payer (institutional/arbitrage) — bears licence denial, stranded assets, and financing exclusion where the standard binds, but hedges by shifting capital and reshaping metric boundaries jurisdiction by jurisdiction
 *   - - coal_mining_communities: Dual-positioned payer/victim (organized/trapped) — supply the highest-mortality pathway's workforce, bear its occupational and environmental death toll, and absorb the standard's concentrated transition costs without adequate compensation
 *   - - fossil_air_pollution_exposed_populations: Primary victim (moderate/constrained) — breathe the combustion toll the standard's arithmetic ranks largest; exposure is ambient and unavoidable at individual level
 *   - - climate_impact_bearing_populations: Secondary victim (powerless/trapped) — bear the probability-weighted damages the standard folds into its totals, represented only as a discount-rate parameter chosen by others
 *   - - nuclear_power_industry: Primary beneficiary (institutional/constrained) — operates the lowest-mortality dispatchable source in the standard's tables; gains wherever the arithmetic governs, exposed to accident shocks everywhere
 *   - - low_carbon_generation_sector: Secondary beneficiary (institutional/mobile) — receives displaced demand, subsidy streams, and carbon-market revenues as fossil pathways are priced out; capital arbitrages between jurisdictions
 *   - - grid_electricity_consumers: Dual-positioned beneficiary/payer (organized/constrained) — receive lower-mortality power and carry compliance costs in tariffs; a volatile electoral constituency for the standard's survival
 *   - - energy_regulators_and_assessment_bodies: Agenda setter (institutional/identity_locked) — administer the mortality tables, valuation parameters, and licensing analyses; their professional office is constituted by the framework they enforce
 *   - - public_health_authorities: Dual-positioned beneficiary/agenda_setter (institutional/identity_locked) — produce and certify the comparative statistics the standard consumes; their mandate and data infrastructure expand with the framework's reach
 *   - - catastrophic_risk_advocates: Excluded voice (organized/mobile) — hold tail-prioritizing commitments, largely absent from the technical venues where acceptability is formally adjudicated
 *   - - analytical_observer: Analytical seat (analytical/analytical) — sees the full structure: the genuine comparison problem solved, the concentrated costs imposed, the value premises buried in the neutral-seeming metric
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.64).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.74).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.64).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable-Risk Standard for Energy Pathways").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '54756aa0-c697-4226-88d0-cb55c3f29454').
narrative_ontology:cs_kernel_codification('54756aa0-c697-4226-88d0-cb55c3f29454', formalized).
narrative_ontology:cs_authority_grounding('54756aa0-c697-4226-88d0-cb55c3f29454', expertise).
narrative_ontology:cs_interpretation_layer_present('54756aa0-c697-4226-88d0-cb55c3f29454').
narrative_ontology:cs_reading_relation('54756aa0-c697-4226-88d0-cb55c3f29454', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('54756aa0-c697-4226-88d0-cb55c3f29454', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('54756aa0-c697-4226-88d0-cb55c3f29454', foundational, expected_harm_minimization_governs_acceptability).
narrative_ontology:cs_axiom_status(expected_harm_minimization_governs_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('54756aa0-c697-4226-88d0-cb55c3f29454', expected_harm_minimization_governs_acceptability, instrumental).
narrative_ontology:cs_axiom('54756aa0-c697-4226-88d0-cb55c3f29454', secondary, statistical_lives_interpersonally_fungible).
narrative_ontology:cs_axiom_status(statistical_lives_interpersonally_fungible, holdable).
narrative_ontology:cs_axiom_grounding('54756aa0-c697-4226-88d0-cb55c3f29454', statistical_lives_interpersonally_fungible, empirically_contingent).
narrative_ontology:cs_reference_frame('54756aa0-c697-4226-88d0-cb55c3f29454', aggregate_expected_harm_minimization_framework).
narrative_ontology:cs_drift_state('54756aa0-c697-4226-88d0-cb55c3f29454', post_fukushima_political_environment, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('54756aa0-c697-4226-88d0-cb55c3f29454', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_power_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, low_carbon_generation_sector).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, grid_electricity_consumers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, public_health_authorities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_air_pollution_exposed_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, coal_mining_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, climate_impact_bearing_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, grid_electricity_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the coal, oil, and gas generation and supply chains that the mortality-based standard singles out for phase-out. They absorb stranded-asset write-downs, licence refusals, and financing exclusions where the standard binds, but hedge extensively: capital shifts into gas, offsets, and rebranded transition portfolios, and lobbying reshapes the metric's inclusion boundaries jurisdiction by jurisdiction. Leaving the field entirely is unnecessary; reshaping the rules they operate under is routine.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_incumbents, payer,
    institutional, biographical, arbitrage, global).

% Supply the workforce behind the highest-mortality pathway: occupational death rates underground, black-lung disease, and regional health burdens from combustion. The same standard that counts their health toll also closes their mines; transition programs arrive late, cover fractions of losses, and rarely match region-specific skills. Housing wealth, family networks, and single-industry local economies anchor them in place.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, coal_mining_communities, payer,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, coal_mining_communities, payer).

% Live downwind of combustion: fine particulates from coal and oil burning drive cardiovascular and respiratory mortality at scales the standard's arithmetic ranks as the largest energy-related death toll. Exposure is ambient — individuals cannot opt out of a regional airshed — and abatement arrives only as fast as pathway substitution, which the standard accelerates where it binds and fails to accelerate where politics overrides it.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_air_pollution_exposed_populations, payer,
    moderate, biographical, constrained, continental).

% Bear the probability-weighted damages — heat mortality, flood, crop stress, displacement — that the standard folds into its expected-harm totals. Their losses arrive on delay and diffuse across geographies; they hold no seat in licensing or tariff proceedings, entering the calculus only as a discount-rate choice made by others.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, climate_impact_bearing_populations, payer,
    powerless, generational, trapped, global).

% Operates the lowest-mortality dispatchable generation source in the standard's tables and gains legitimacy, licence renewals, and finance wherever the arithmetic governs. Its assets are site-bound and its fortunes swing with each accident abroad: a single meltdown anywhere resets public tolerance everywhere, and the industry has no exit from that coupling.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_power_industry, beneficiary,
    institutional, generational, constrained, global).

% Wind, solar, hydro, and storage developers receive the displaced demand: capacity auctions, subsidy streams, and carbon-market revenues route toward them as fossil pathways are priced out. Capital is mobile across jurisdictions and subsidy regimes, letting them arbitrage the differences in how aggressively each country applies the standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, low_carbon_generation_sector, beneficiary,
    institutional, biographical, mobile, global).

% Receive power with a shrinking mortality footprint and, where markets function, stable prices; they also carry compliance costs passed through in tariffs and taxes funding the transition. Their recourse is electoral and intermittent — they reward cheap power and punish visible price shocks, which makes them a volatile constituency for the standard's continuation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, grid_electricity_consumers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, grid_electricity_consumers, payer).

% Administer the machinery: mortality tables, value-of-statistical-life parameters, licensing analyses, integrated assessment models. Their professional standing and procedural authority are built on the commensuration framework; abandoning it would dissolve the epistemic office they occupy, so they defend its parameters even where they privately acknowledge boundary disputes.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_regulators_and_assessment_bodies, agenda_setter,
    institutional, generational, identity_locked, national).

% Produce and certify the comparative mortality statistics the standard consumes — burden-of-disease work, national health assessments. The standard extends their mandate from treating illness to ranking entire energy systems; their data infrastructure and budget lines grow with the framework's reach, and their methodological conventions quietly set its inclusion boundaries.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, public_health_authorities, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, public_health_authorities, agenda_setter).

% Hold that low-probability, high-consequence events — reactor meltdowns, waste-isolation failures, proliferation — deserve overriding weight regardless of expected-value arithmetic. They litigate, campaign, and legislate at the margins but are largely absent from the technical venues where acceptability is formally adjudicated, entering the record mainly as intervenors after decisions are drafted.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_risk_advocates, excluded,
    organized, biographical, mobile, global).

% Holds no stake in any pathway; observes the full structure — the genuine comparison problem the standard solves, the concentrated costs its operation imposes, the value premises buried in its neutral-seeming metric, and the rival criteria waiting at the margins.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, low_carbon_generation_sector).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single commensurating metric — deaths per terawatt-hour, aggregated into expected-harm totals — that lets heterogeneous pathway risks (radiological, combustive, climatic, structural) be compared on one scale and steers investment and regulatory tolerance toward the least-harmful sources. Without it, pathway comparison collapses into incomparable dread narratives and incumbent advantage.
% TRANSFER_FUNCTION: Moves investment, regulatory tolerance, and moral attention from high-mortality pathways (coal, oil) toward low-mortality ones (nuclear, hydro, renewables); moves concentrated transition costs onto fossil-dependent workers and regions; moves decision authority toward holders of analytic expertise who operate the metric.
% ABSENT_VOICES: Catastrophic-tail advocates and option-value pluralists are largely absent from the technical venues — regulatory impact analysis, integrated assessment modeling, licensing boards — where acceptability is formally adjudicated; they appear as post-hoc intervenors. Fossil-dependent community members rarely sit on the assessment panels that price their regions' transition. Future generations appear only as a discount-rate parameter chosen by present parties.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, pathway decisions would reorganize around whichever rival criterion captured the vacuum: tail-dominant framing would deepen nuclear retrenchment and fossil-phaseout skepticism alike; option-value framing would impose portfolio mandates and block pathway consolidation. Licensing doctrine, carbon-pricing justification, divestment screens, and the comparative-risk literature's policy uptake all presuppose the commensurating framework — investment signals across the entire energy sector would re-sort.
% FOUNDING_PROBLEM: Post-war expansion of civilian energy produced heterogeneous, mutually incomparable risks — radiological dread, combustion mortality, dam failure, later climate accumulation — with no shared basis for declaring any of them tolerable. Early reactor-siting conflicts and air-quality crises demanded a common metric that could rank a meltdown against a mine disaster against a smog season.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the public-health literature (comparative risk assessments in the Lancet and WHO burden-of-disease work, authored independently of nuclear and fossil commercial interests), insurance and reinsurance actuarial practice (which prices pathway risk without adopting the standard's normative conclusions), and environmental-economics scholarship critical of both this standard and its rivals all attest that the cross-pathway comparison problem is real and unsolved by any alternative framework. Industry attestations from both nuclear and fossil sides are self-interested and not relied upon.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64: substantial but bounded. The standard's operation transfers real value — market share, subsidy streams, decision authority — toward positioned beneficiaries while imposing concentrated, largely uncompensated costs on trapped fossil-dependent communities, and its own arithmetic indicts its actual operation (by its lights, millions of avoidable combustion deaths persist because its suppression of the fossil pathway is politically defeasible). It is not higher because the standard also demonstrably retires the deadliest pathway where it binds, and its victims retain partial recourse. Suppression is authored at 0.74 as a raw structural property (unscaled by power or scope): the standard holds down the fossil pathway through carbon pricing, mortality-based licensing denial, divestment pressure, and finance exclusion — the reading's declared structural signature. Theater ratio 0.33: the analytic machinery is substantially functional (real comparative epidemiology, real actuarial practice) but a growing share is commissioned advocacy and post-accident rhetorical deployment rather than prospective decision-making. Accessibility collapse 0.42: rival decision frameworks remain legally and politically live (precautionary doctrine, tail-focused litigation, portfolio mandates); the standard raises their cost of entry in technical venues without collapsing them. Resistance 0.58: sustained — fossil-dependent regional politics, industry-funded counter-analysis, post-accident public reversals, and energy-price backlash. MEASUREMENTS: one shared time grid (t=0..35 mapping five-year steps 1990-2025) with all three tracked metrics authored at every point. base_extractiveness rises as the standard's footprint grows and its unachieved arithmetic accumulates by its own lights, with a plateau across the Fukushima-paralysis window (t=20-25). theater_ratio rises with the commissioned-analysis industry, easing slightly as open mortality datasets tightened quality. suppression_requirement is tracked because enforcement-capacity change IS this story's traced dynamic: the standard's enforcement machinery visibly built out (emissions trading from 2005, divestment from 2012, finance exclusion, border adjustment mechanisms), spiking defensively after 2011 and easing marginally as renewable cost-competitiveness began carrying suppression without regulatory push.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine should see that divergence. From the agenda_setter seats (regulators, public-health authorities), the arrangement is the framework they are professionally constituted by — identity-locked beneficiaries of their own administration, experiencing the standard as neutral method. From the trapped payer seats (coal communities, ambient-exposed populations), the same structure operates as concentrated uncompensated charge: their exit options are regional roots and ambient airsheds, so their effective position sits near the full-target end. The fossil incumbents' seat diverges sharply from other payers: nominally charged, their arbitrage-grade exit (capital mobility, offset purchases, jurisdictional metric-shopping) damps their effective extraction well below what their payer role alone would predict — the standard extracts least from those best able to reshape it. Consumers sit near symmetric: genuine product benefit, diffuse cost pass-through. The excluded advocates experience the arrangement as a closed venue rather than a charge — their grievance is procedural absence, which the engine reads through the excluded role rather than through directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (nuclear industry, low-carbon sector, consumers, public-health authorities) derive low directionality — the standard subsidizes them through market reallocation and mandate expansion. Victim declarations (combustion-exposed populations, coal communities, climate-damage bearers) derive high directionality, amplified by trapped and constrained exit options: ambient exposure and regional lock-in sit near the full-target end. The dual-positioned seats split: coal communities are simultaneously victims of the tolerated pathway's mortality and payers of the transition it forces — their net directionality is high, and their trapped exit prevents the damping arbitrage affords incumbents. Regulators and public-health authorities are pinned to the beneficiary side by identity-lock: their professional offices are constituted by the framework, so their structural relationship to the standard is custodial-beneficial regardless of nominal analytic neutrality. Incumbents' arbitrage exit pulls their derived directionality down from the payer baseline — the derivation chain captures this from exit_options without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no common basis for comparing heterogeneous energy risks (radiological dread, combustion mortality, dam failure, climate accumulation) — remains live: every new pathway (SMRs, fusion, storage fires, hydrogen) reintroduces it. Mandatrophy is therefore NOT resolved, and no sunset clause is declared. The tangled_rope classification is what prevents mislabeling in both directions: a pure-coordination reading would erase the concentrated uncompensated losses, the gameability of the metric's boundaries, and the enforcement dependence (the standard must be re-defended after every visible accident, and loses those defenses often enough that its suppression of the fossil pathway is chronically incomplete by its own arithmetic); a pure-extraction reading would erase the genuine achievement — commensuration solved a real collective-action problem, and where the standard binds, the deadliest pathway in the mix actually retires. The classification keeps both truths load-bearing and marks the drift vectors: closure of the compensation gap drifts toward rope; deepening metric capture by positioned interests drifts toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_weighting_dispute,
    'This constraint is one reading of the acceptable_risk_energy kernel: acceptability as minimized aggregate expected harm. Sibling readings (catastrophic_tail_dominant, option_value_preserving) weight the same outcome distributions differently — what changes structurally if a sibling reading governs instead?',
    'Not resolvable by data alone: the dispute is located in the weighting function over outcome distributions (probability-weighted aggregate vs. tail-avoidance priority vs. flexibility preservation), not in the mortality measurements themselves. Resolution would require an explicit meta-criterion for choosing between decision criteria, which no party currently accepts.',
    'Under catastrophic_tail_dominant, nuclear-accident victims enter the victim set at full dread-weighted magnitude and suppression targets the nuclear pathway rather than the fossil pathway; under option_value_preserving, foreclosed flexibility becomes the primary harm. Classification of the governing arrangement could shift from tangled_rope toward snare if suppression concentrates on a low-mortality pathway under dread politics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_weighting_dispute, conceptual, 'Committer structure: which weighting criterion governs acceptable risk determines victim composition and suppression target.').

omega_variable(
    mortality_boundary_ambiguity,
    'Mortality-per-TWh figures depend on inclusion boundaries: are downstream climate deaths counted, is the linear-no-threshold or threshold dose-response model used for radiation, are mining and construction deaths attributed to the pathway, how are conflict-driven emissions attributed?',
    'Standardized attribution protocol audited by a body outside both fossil and nuclear interests (IHME-style boundary declaration with sensitivity bands published alongside point estimates).',
    'Narrow boundaries lower measured fossil mortality, weakening the warrant for the standard''s high suppression of the fossil pathway and lowering measured extraction; broad boundaries raise both. Because different boundaries yield different epsilon for the same colloquial label, unresolved boundary ambiguity threatens the epsilon-invariance of this story and argues for eventual decomposition into boundary-explicit sibling stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mortality_boundary_ambiguity, empirical, 'Whether the metric''s apparent precision conceals contestable inclusion boundaries that move epsilon.').

omega_variable(
    statistical_life_fungibility,
    'Does the standard''s treatment of statistical lives as fungible across exposure contexts (voluntary occupational risk vs. involuntary ambient exposure vs. imposed long-term climate risk) survive scrutiny, or does it mask a value imposition on those exposed without consent?',
    'Distributional-weighting research (survey and experimental economics on WTP/WTA asymmetries across exposure types) plus legal doctrine on non-consensual risk imposition.',
    'If fungibility fails, the coordination function partially covers a value imposition: the extraction component rises and the arrangement drifts toward the snare boundary. If fungibility holds as a workable approximation, the arrangement drifts toward the rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_life_fungibility, conceptual, 'Whether the neutrality premise buried in the commensurating metric is a workable approximation or a covered imposition.').

omega_variable(
    enforcement_defeat_attribution,
    'Is the measured extraction a property of the standard itself, or of the political environment that repeatedly defeats it (post-accident dread surges that shut low-mortality capacity and raise near-term combustion, as after Fukushima in 2011)?',
    'Comparative jurisdiction study: jurisdictions where expected-value framing held across accident shocks (France''s historical nuclear commitment) versus jurisdictions where it collapsed (Germany, Japan post-2011), holding mortality data constant.',
    'If extraction tracks the political environment rather than the standard''s intrinsic operation, the standard''s intrinsic epsilon is lower than measured and the binding limitation is the strength of sibling-framing politics — an omega-routed committer effect rather than a property of this reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_defeat_attribution, empirical, 'Attribution of the extraction level between the standard''s structure and its hostile reception environment.').

omega_variable(
    transition_compensation_adequacy,
    'Are the concentrated losses borne by fossil-dependent workers and regions compensable in principle (making the charge a correctable feature), or structurally uncompensable given regional skill specificity, housing lock-in, and single-industry community identity?',
    'Longitudinal evaluation of compensated-transition programs (German coal-exit payments, EU Just Transition Fund disbursements) against baseline counterfactuals for regional income, employment, and health trajectories.',
    'Demonstrably adequate compensation would remove the concentrated-uncompensated-loss component and drift the arrangement toward pure coordination; demonstrated failure entrenches the extraction component and hardens the tangled_rope classification toward the snare boundary over the next measurement interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_compensation_adequacy, empirical, 'Whether the concentrated costs landing on fossil-dependent communities are a fixable distributional defect or a structural feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.22).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__expected_value_dominant, theater_ratio, 5, 0.25).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__expected_value_dominant, theater_ratio, 10, 0.28).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__expected_value_dominant, theater_ratio, 15, 0.31).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__expected_value_dominant, theater_ratio, 20, 0.34).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__expected_value_dominant, theater_ratio, 25, 0.37).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__expected_value_dominant, theater_ratio, 30, 0.35).
narrative_ontology:measurement(acce_tr_t35, acceptable_risk_energy__expected_value_dominant, theater_ratio, 35, 0.33).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(acce_be_t35, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 35, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(acce_su_t35, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 35, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel 'acceptable risk in energy policy' decomposes into three criterion-constraints (expected_value_dominant, catastrophic_tail_dominant, option_value_preserving) sharing one referent — the operative acceptable-risk regime — with different epsilon values, victim compositions, and suppression targets. This reading is upstream empirically: its mortality-per-TWh substrate is the common evidentiary ground the sibling readings contest, and its tables shape what tail- and option-oriented critics must argue against. Downstream edges run from this story to both siblings; the siblings' files should carry reciprocal edges and their own dual-formulation notes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
