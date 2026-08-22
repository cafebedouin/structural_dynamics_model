% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Legitimacy Gate for Climate Mitigation Technologies
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel -
 *   what makes a technology legitimate for climate mitigation - is read here
 *   through the precautionary criterion: legitimacy if and only if worst-case
 *   failure modes and legacy costs are bounded and reversible within a
 *   generation. The standing arrangement under contest, and the epsilon
 *   referent, is the legitimacy-screening regime as it actually operates in
 *   climate finance and permitting: taxonomy rules, subsidy eligibility
 *   tests, divestment pressure, and regulatory weighting that together deny
 *   legitimacy to trans-generational-legacy technologies and confer it on
 *   generation-scale-reversible ones. Per the epsilon-referent rule, epsilon
 *   is authored for THAT arrangement as this reading sees it - not for the
 *   velocity- or reliability-optimal arrangements the sibling readings would
 *   install. The sibling readings are separate constraint files linked via
 *   network.affects_constraints; their objections are routed to omega
 *   variables, not folded into this classification. Structural delta specific
 *   to this reading: renewables enter the beneficiary set because
 *   decommissioning closes within a generation; fission is excluded because
 *   waste stewardship and accident legacies do not; future generations appear
 *   on both sides - protected prospectively, burdened with the
 *   already-committed stock.
 *
 * KEY AGENTS:
 *   - renewable_energy_sector: Primary beneficiary (organized/mobile) - clears the reversibility screen, collects the legitimacy premium in subsidies, taxonomy status, and capital cost
 *   - nuclear_industry_and_supply_chain: Primary target (organized/constrained) - fails the screen on legacy and accident grounds regardless of carbon performance; bears capital penalties and stranded order books
 *   - future_generations: Dual-positioned (powerless/trapped) - prospectively protected by the screen, simultaneously holders of the already-committed irreversible stock, with no seat in the arrangement
 *   - climate_finance_standard_setters: Agenda setter (institutional/arbitrage) - translates the criterion into binding capital-market rules; institutional standing rides on the screen's perceived principledness
 *   - nuclear_host_communities: Secondary payer (moderate/constrained) - absorb the localized decline of sidelined facilities
 *   - developing_nation_energy_planners: Excluded voice (moderate/constrained) - bound by capital conditions written in forums they do not occupy
 *   - energy_systems_analysts: Analytical observer (analytical/analytical) - produce the cost figures both sides cite; hold no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Legitimacy Gate for Climate Mitigation Technologies").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, 'e0c7a23d-a649-43c7-a9c3-04d66e11add3').
narrative_ontology:cs_kernel_codification('e0c7a23d-a649-43c7-a9c3-04d66e11add3', formalized).
narrative_ontology:cs_authority_grounding('e0c7a23d-a649-43c7-a9c3-04d66e11add3', expertise).
narrative_ontology:cs_interpretation_layer_present('e0c7a23d-a649-43c7-a9c3-04d66e11add3').
narrative_ontology:cs_reading_relation('e0c7a23d-a649-43c7-a9c3-04d66e11add3', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0c7a23d-a649-43c7-a9c3-04d66e11add3', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('e0c7a23d-a649-43c7-a9c3-04d66e11add3', foundational, irreversibility_disqualifies_legitimacy).
narrative_ontology:cs_axiom_status(irreversibility_disqualifies_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e0c7a23d-a649-43c7-a9c3-04d66e11add3', irreversibility_disqualifies_legitimacy, deontological).
narrative_ontology:cs_axiom('e0c7a23d-a649-43c7-a9c3-04d66e11add3', secondary, worst_case_risk_bounds_acceptability).
narrative_ontology:cs_axiom_status(worst_case_risk_bounds_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('e0c7a23d-a649-43c7-a9c3-04d66e11add3', worst_case_risk_bounds_acceptability, empirically_contingent).
narrative_ontology:cs_reference_frame('e0c7a23d-a649-43c7-a9c3-04d66e11add3', intergenerational_reversibility_stewardship).
narrative_ontology:cs_drift_state('e0c7a23d-a649-43c7-a9c3-04d66e11add3', post_2022_energy_security_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e0c7a23d-a649-43c7-a9c3-04d66e11add3', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_and_supply_chain).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_host_communities).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, precautionary_principle_doctrine).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, intergenerational_equity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and operates solar, wind, storage, and efficiency assets. Its worst-case failures - panel disposal, blade landfilling, mine tailings from supply chains - are costly but close out within decades through decommissioning bonds, land restoration, and recycling pathways, so its technologies clear the reversibility screen. It collects eligibility for green subsidies, taxonomy inclusion, preferential procurement, and measurably cheaper capital. Capital and project pipelines can be redeployed across jurisdictions if any single market tightens.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, global).

% Builds and operates fission plants delivering large volumes of near-zero-carbon electricity. Its spent fuel requires active stewardship on horizons far beyond a generation and its severe accidents produce exclusion zones lasting generations, so it fails the screen regardless of its carbon performance. It loses taxonomy status, pays persistent capital penalties, and faces shrinking order books in screen-governed markets. Reactor sites, fuel-cycle facilities, and a licensed specialist workforce cannot relocate or convert to other lines of work.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_and_supply_chain, payer,
    organized, biographical, constrained, global).

% Will inherit whatever legacies today's technology choices commit. Prospectively, the screen shields them from newly created irreversible burdens: no new trans-generational waste streams or permanent sacrifice zones get legitimacy approval. At the same time they hold the already-committed stock - long-lived waste awaiting disposal, committed warming, unfunded decommissioning liabilities - which the forward-looking screen neither remediates nor compensates. They hold no seat, vote, or representative in any body administering the screen, and exit from the planet's inherited arrangements is not a coherent option for them.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, future_generations, payer).

% Draft and maintain taxonomy criteria, disclosure rules, and eligibility screens that translate the reversibility test into capital-market rules. They take testimony from industry, scientific advisors, and member governments, and can amend thresholds under political pressure - as occurred when nuclear and gas were conditionally readmitted to some frameworks after 2022. Their institutional standing depends on the screen being perceived as principled risk administration rather than lobbied preference, and their personnel circulate between standard-setting bodies, ministries, and finance.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, climate_finance_standard_setters, agenda_setter,
    institutional, generational, arbitrage, global).

% Towns and regions built around reactor sites, enrichment and fuel-fabrication facilities, and their payrolls. As orders shrink and phaseout dates harden, they face managed decline: property values, school enrollments, and local tax bases ride on facilities the screen sidelines. Leaving means abandoning place-specific skills, housing wealth accumulated around plant employment, and community ties; staying means absorbing the decline.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_host_communities, payer,
    moderate, biographical, constrained, regional).

% Plan electrification under tight budgets and steep demand growth. Green concessional finance increasingly arrives conditioned on screen-compliant portfolios, narrowing the technology menu they can afford. They sit outside the standard-setting bodies that write the conditions yet must operate inside them; their objections that the screen raises their decarbonization costs and delays grid buildout rarely reach the drafting tables.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, developing_nation_energy_planners, excluded,
    moderate, biographical, constrained, national).

% Run integrated assessment and capacity-expansion models comparing system cost, reliability, and emissions under different technology admissibility rules. They publish the cost-of-exclusion and cost-of-inclusion figures that both sides of the dispute cite, and hold no vote in any administering body, but their scenario assumptions shape what the disputing parties treat as the factual baseline.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, energy_systems_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, renewable_energy_sector).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, enforceable standard for which mitigation technologies may claim climate legitimacy, solving a real collective-action problem: without a common admissibility test, each actor accepts whatever legacy costs fit its own time horizon and the cleanup bills arrive late, diffusely, and on parties who never chose them.
% TRANSFER_FUNCTION: Moves legitimacy, capital access, and permitting permission toward technologies whose worst-case failures resolve within a generation (renewables, efficiency, storage) and away from technologies carrying trans-generational legacy profiles (fission). Concretely: subsidy eligibility, taxonomy status, and lower costs of capital flow from nuclear-aligned portfolios to renewable-aligned ones.
% ABSENT_VOICES: Future generations hold no seat anywhere in the arrangement, though they are the parties the screen most affects on both sides - protected prospectively, burdened with the inherited stock, and unable to object, litigate, or renegotiate. Developing-nation planners are bound by capital conditions written in forums they do not occupy. Nuclear-host regions have no procedural venue where their transition costs are weighed against the abstract reversibility gains the screen delivers elsewhere.
% DISAPPEARANCE_RATIONALE: If the screen vanished overnight, green taxonomies would lose their exclusion logic, capital would reprice nuclear upward and strip the legitimacy premium from renewables, jurisdictions with legislated nuclear phaseouts would face immediate reversal pressure, and the norm against socializing trans-generational cleanup costs would have to be rebuilt from scratch. The renewable buildout pipeline, the divestment architecture, and the taxonomy compliance industry all presuppose the screen's continuing operation.
% FOUNDING_PROBLEM: The screen was built to stop climate policy from repeating the pattern of earlier technological commitments - civilian nuclear waste, ozone-depleting refrigerants, leaded fuels - in which benefits accrued immediately and locally while worst-case costs arrived late, diffusely, and to parties who had no say in the original decision.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by: the international treaty lineage that codified the precautionary principle (Rio Principle 15 and its descendants), insurance and actuarial industries that price long-tail nuclear liability and refuse full coverage, and national radioactive-waste management agencies whose own published cost horizons extend centuries past a generation. Stated plainly: no one speaks from inside the position of future generations - corroboration attests the problem's reality, not the affected parties' consent.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. Claimed type is tangled_rope because the structure genuinely satisfies both halves: a real coordination function (a common admissibility standard preventing a race to socialize trans-generational cleanup costs - a problem no market actor internalizes voluntarily) AND asymmetric transfer through the same structure (legitimacy, subsidy eligibility, and cheap capital move from fission-aligned to renewable-aligned portfolios, with identifiable losers). Active enforcement is required: without taxonomy regulations, disclosure mandates, and finance screens, the criterion is a lecture, not a gate. Metrics: extractiveness 0.58 - substantial asymmetric transfer, tempered by the genuine protective good delivered; suppression 0.62 - the gate operates through finance and taxonomy exclusion rather than prohibition, but within adopting jurisdictions the effect approaches prohibition and alternatives (building fission elsewhere, redesigning for bounded failure) are only partly available; theater_ratio 0.38 - the core test is functional, but a growing share of its application is selective scrutiny that reaches preferred conclusions (see omega selective_scrutiny_motivation); accessibility_collapse 0.40 - understanding the gate does not end a proponent's options, since designs can be argued across the threshold; resistance 0.62 - sustained, organized contestation from nuclear states, industrial interests, energy-security advocates, and excluded developing-economy planners. The measurement series share one time grid (1992, 2000, 2008, 2015, 2020, 2024) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity hardening from a declaratory principle (Rio 1992) through national renewable mandates, divestment architecture, and binding taxonomy delegated acts - the gate's coercive machinery was built up over the interval, which a static scalar would miss. Receipt surface: gain_flow names renewable_energy_sector because the gains demonstrably accrue there - taxonomy inclusion and subsidy eligibility convert directly into that seat's financing costs and order books; no other seat captures comparable value (standard setters collect standing, not the transfer itself). fixing_cost is prohibitive: the standard setters who could unwind the screen would collapse the greenwashing defense their mandates rest on, invite capture findings, and trigger the litigation that greeted every prior threshold amendment - the cost to them exceeds any benefit they would collect from removal.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types from identical structural data. From the renewable_energy_sector seat, the gate is the thing that makes its business case: a principled boundary that rewards reversible technologies - coordination all the way down. From the nuclear_industry_and_supply_chain seat, the same gate is a legitimacy denial machine that ignores its carbon performance and prices its exit impossibility. From the climate_finance_standard_setters seat, it is neutral risk administration - a threshold applied to dossiers. From the future_generations seat, it is simultaneously a shield (prospective) and an unpaid invoice (inherited stock). The engine computes these per-seat classifications from the power, exit, and role data; the divergence between the beneficiary seat's coordination experience and the payer seat's extraction experience is the measured quantity, not something the authored claim resolves.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: renewable_energy_sector (sole beneficiary among seated actors, mobile exit) derives near the beneficiary pole; nuclear_industry_and_supply_chain (victim, constrained exit, site-specific assets) derives near the target pole with amplification from its exit trap; nuclear_host_communities derive as diffuse-local targets. Two overrides are declared where the derivation chain cannot see the true relationship. First, future_generations appears in BOTH the beneficiary and victim arrays - the mechanical derivation cannot resolve a dual-listed agent, and the honest net position under this reading's own accounting is symmetric: prospective protection roughly balances uncompensated inheritance of the committed stock, so d=0.5 for the powerless atom (which uniquely identifies this agent in the story). Second, climate_finance_standard_setters holds no beneficiary or victim declaration (they administer rather than collect), so the canonical fallback would misplace them; their institutional standing and mandate expansion ride on the screen's continuation, giving a mild beneficiary tilt of d=0.2 for the institutional atom (uniquely theirs). Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - stopping climate policy from committing new trans-generational legacies - is live: new reactor builds, capture scale-up proposals, and geoengineering research keep arriving, and the committed stock keeps growing. No mandatrophy is declared, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges, so no zombie flag fires. The tangled_rope classification does the preventive work in both directions: calling this a rope would erase the identifiable losers (an organized industry paying capital penalties for a property of its technology rather than its carbon performance, host communities in managed decline, excluded developing-economy planners); calling it a snare would erase the genuine stewardship function that no alternative mechanism currently performs - the collective-action problem it solves is real, and its beneficiaries include a party with no ability to pay for the service. The piton signature is also guarded against: although theater is present (selective scrutiny), the gate's administrator could not cheaply discard it without self-harm, and concentrated gain receipt (renewable_energy_sector) disqualifies the no-capturer piton cell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (precautionary_reading) of the technology_legitimacy_kernel. Would the sibling readings (reliability_primacy_reading, velocity_primacy_reading) classify the same governance arrangement differently, and where exactly does the disagreement bite?',
    'Comparative classification of the sibling stories against this one. The disagreement is located in the foundational axiom: this reading holds irreversibility_disqualifies_legitimacy; the reliability sibling holds that dispatchability confers legitimacy; the velocity sibling holds that in-budget deployability confers it. Whichever axiom governs the major taxonomies determines the partition.',
    'If the velocity reading prevails institutionally, nuclear re-enters the beneficiary set, the renewables legitimacy premium compresses, and this story''s victim structure partially inverts. If the reliability reading prevails, both this reading''s beneficiaries and the velocity reading''s beneficiaries lose standing to dispatchable assets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints with different epsilon and partitions.').

omega_variable(
    reversibility_threshold_placement,
    'What counts as ''bounded and reversible within a generation''? Small modular reactor advocates claim passively safe designs with decades-scale waste take-back change the answer; deep geological repositories claim the waste problem is bounded if not reversible; carbon capture permanence sits ambiguously. Where the threshold lands decides which technologies cross the gate.',
    'Adjudicated engineering review of specific design claims against a stated reversibility definition - what fraction of worst-case cost must be recoverable, on what evidence, at what discount for monitoring burden.',
    'Threshold placement moves entire technology classes across the gate line, redrawing the beneficiary and victim sets and shifting epsilon materially in either direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_threshold_placement, empirical, 'The gate''s operative threshold is under-specified and contestable per technology.').

omega_variable(
    selective_scrutiny_motivation,
    'Is the asymmetric scrutiny - strict reversibility demanded of fission while the mining, manufacturing, and biodiversity externalities of favored technologies receive lighter treatment - principled (reversibility genuinely differs across classes) or motivated (the test is applied to reach preferred conclusions)?',
    'Audit of screening decisions against a uniform externality ledger: compare the rigor applied to nuclear waste legacies versus solar supply-chain and wind end-of-life legacies at equivalent cost magnitudes.',
    'If the motivated component dominates, the theater_ratio is understated and the gate drifts toward a coordination-cover reading of its own operation; if principled, the current theater estimate stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_scrutiny_motivation, empirical, 'Whether the gate''s uneven application reflects the underlying physics or advocacy pressure.').

omega_variable(
    fg_net_position,
    'Which term dominates for future generations: prospective protection from newly created irreversible burdens (beneficiary-side) or uncompensated inheritance of the already-committed legacy stock (target-side)?',
    'Valuation comparison of marginal protection delivered by the screen against the undiscounted stewardship and adaptation burdens already committed, using intergenerational accounting rather than discounted utilitarian frames.',
    'If the inherited-stock term dominates, future generations flip from beneficiary-side to target-side directionality, raising computed effective extraction and pressuring the classification toward the extractive end; the current symmetric override (d=0.5) would be replaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fg_net_position, conceptual, 'Net structural position of the only party with no seat in the arrangement.').

omega_variable(
    exclusion_mitigation_cost,
    'Does excluding firm low-carbon generation from legitimacy materially raise the cost or lengthen the timeline of decarbonization in screen-governed jurisdictions, or do renewables-plus-storage trajectories make the exclusion approximately cost-neutral?',
    'Counterfactual capacity-expansion modeling with and without the admissibility restriction, validated against realized buildout data from jurisdictions that diverge on the screen (France versus Germany versus South Korea).',
    'If exclusion materially slows mitigation, present populations exposed to residual warming join the effective victim set and epsilon rises above the authored value; if cost-neutral, the current estimate stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_mitigation_cost, empirical, 'Whether the gate''s protective function carries a material mitigation-speed price.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1992, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 1992, 0.14).
narrative_ontology:measurement(tech_tr_t2000, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(tech_tr_t2008, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(tech_tr_t2015, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(tech_tr_t2020, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(tech_tr_t2024, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(tech_be_t1992, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 1992, 0.28).
narrative_ontology:measurement(tech_be_t2000, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(tech_be_t2008, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2008, 0.41).
narrative_ontology:measurement(tech_be_t2015, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2015, 0.49).
narrative_ontology:measurement(tech_be_t2020, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(tech_be_t2024, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1992, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 1992, 0.18).
narrative_ontology:measurement(tech_su_t2000, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2000, 0.27).
narrative_ontology:measurement(tech_su_t2008, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2008, 0.39).
narrative_ontology:measurement(tech_su_t2015, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(tech_su_t2020, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement(tech_su_t2024, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, identity_coordination).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'a legitimate climate technology' is a contested kernel, not one constraint. It decomposes into at least three rival criteria readings - this precautionary reading (reversibility of worst-case failure and legacy), a reliability-primacy reading (dispatchable baseload capability), and a velocity-primacy reading (deployability within the remaining carbon budget). Each reading instantiates a different constraint with its own epsilon, beneficiary/victim partition, and classification; this file authors only the precautionary reading. The siblings are separate stories linked here. Upstream/downstream dynamic: whichever reading governs the major taxonomies changes the resource availability of the others - exclusion of fission from green finance strengthens the velocity reading's empirical complaint while leaving its logical standing untouched.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, powerless, 0.5).
constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
