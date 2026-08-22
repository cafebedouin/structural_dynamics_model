% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of the Climate Response Obligation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation-priority reading of the climate
 *   response obligation kernel: the claim that preventing future harm through
 *   rapid decarbonization is the correct discharge of intergenerational
 *   justice, as against readings that would accept substantial warming and
 *   invest in adaptation, or that would prioritize reducing material
 *   throughput over emissions-focused efficiency. Under this reading, future
 *   generations and currently climate-vulnerable populations are the
 *   beneficiaries of an obligation whose costs fall disproportionately on
 *   Global North governments and taxpayers (by virtue of historical emissions
 *   responsibility) and on fossil capital and fossil-dependent workers (by
 *   virtue of asset and livelihood devaluation). The obligation requires
 *   active enforcement — carbon pricing, phase-out mandates, disclosure
 *   regimes, international finance commitments — to hold against resistance
 *   from parties whose capital or employment it devalues. Declining theater
 *   ratio over the interval reflects the shift from largely rhetorical early
 *   commitments (Rio 1992, Kyoto's weak enforcement) toward increasingly
 *   binding, litigated, and market-priced mechanisms (Paris Agreement ratchet
 *   mechanism, EU carbon border adjustment, divestment campaigns) — the
 *   coordination function has become more real even as extraction and
 *   required suppression have both risen.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.47).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Mitigation-Priority Reading of the Climate Response Obligation").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc').
narrative_ontology:cs_kernel_codification('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', distributed).
narrative_ontology:cs_authority_grounding('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', distributed).
narrative_ontology:cs_reading_relation('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', foundational, prevention_of_future_harm_takes_precedence_over_transition_cost).
narrative_ontology:cs_axiom_status(prevention_of_future_harm_takes_precedence_over_transition_cost, holdable).
narrative_ontology:cs_axiom_grounding('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', prevention_of_future_harm_takes_precedence_over_transition_cost, deontological).
narrative_ontology:cs_axiom('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', foundational, historical_emitters_bear_disproportionate_mitigation_burden).
narrative_ontology:cs_axiom_status(historical_emitters_bear_disproportionate_mitigation_burden, holdable).
narrative_ontology:cs_axiom_grounding('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', historical_emitters_bear_disproportionate_mitigation_burden, conventional).
narrative_ontology:cs_axiom('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', secondary, carbon_budget_scarcity_requires_near_term_decarbonization_rate).
narrative_ontology:cs_axiom_status(carbon_budget_scarcity_requires_near_term_decarbonization_rate, holdable).
narrative_ontology:cs_axiom_grounding('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', carbon_budget_scarcity_requires_near_term_decarbonization_rate, empirically_contingent).
narrative_ontology:cs_reference_frame('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', unfccc_common_but_differentiated_responsibility_framework).
narrative_ontology:cs_drift_state('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', post_paris_ratchet_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('144f3ee0-f3be-42b1-a84c-ee65f8c1f1dc', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, renewable_energy_industry).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_capital).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_dependent_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_taxpayers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, energy_intensive_industry_in_transition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_north_taxpayers).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, polluter_pays_principle).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, common_but_differentiated_responsibilities_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot participate in current decisions but inherit whatever climate stability or instability current policy produces. Under this reading they are the primary referent of obligation: every tonne of avoided warming is framed as a transfer of livable conditions to them. They have no seat, no vote, and no capacity to object to insufficient action or to the costs imposed on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Face the earliest and most severe physical impacts of warming despite minimal historical emissions. Benefit in principle from aggressive mitigation but have little enforcement power over Global North commitments; often excluded from the rooms where mitigation targets and financing are actually set, despite being the constituency the obligation is nominally for.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_nations, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, climate_vulnerable_nations, excluded).

% Captures subsidies, mandates, and procurement preferences created to accelerate decarbonization. Genuinely advances the coordination goal while also profiting substantially from the regulatory architecture that mandates its product; can relocate capital across jurisdictions to chase the most favorable mitigation-policy regime.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, renewable_energy_industry, beneficiary,
    organized, biographical, mobile, global).

% Set decarbonization targets, carbon pricing, and international commitments; administer enforcement through regulation and treaty obligation. Bear the diplomatic and fiscal burden of financing global mitigation given historical emissions responsibility, but also control the pace and design of the transition, including how much cost is passed to domestic populations versus absorbed as sovereign expenditure.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_governments, agenda_setter,
    institutional, generational, constrained, global).

% Holds reserves and infrastructure whose value depends on continued extraction; rapid decarbonization directly targets these assets for stranding through phase-out mandates, carbon pricing, and financing bans. Has resources to lobby and litigate but the mitigation-priority reading treats its capital as the thing that must be devalued for the obligation to be met — it cannot exit the constraint by relocating, only by converting or losing the asset.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_capital, payer,
    powerful, biographical, constrained, global).

% Employed in extraction, refining, and fossil-fired generation in specific regions where these are the dominant employers. Bear job loss and community decline as mitigation policy closes facilities, often without adequate transition financing reaching them despite formal 'just transition' commitments. Cannot easily relocate skills or geography on the timeline mitigation requires.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_dependent_workers, payer,
    powerless, biographical, trapped, regional).

% Fund domestic decarbonization subsidies, carbon pricing revenue recycling, and international climate finance commitments through taxes and higher energy prices during the transition. Also benefit from avoided future climate damages and cleaner air, but the redistribution from taxpayer to mitigation infrastructure is immediate while the benefit is diffuse and delayed.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_taxpayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_north_taxpayers, beneficiary).

% Steel, cement, chemicals, and heavy manufacturing face higher input costs and capital expenditure requirements to decarbonize processes on mandated timelines. Can lobby for carveouts or relocate production to jurisdictions with weaker mitigation requirements (carbon leakage), but this undermines the emissions-reduction goal the obligation exists to serve.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, energy_intensive_industry_in_transition, payer,
    powerful, biographical, constrained, national).

% Argue for a right to development pathways that historical emitters already used, and for financing commensurate with the differentiated-responsibility principle the reading itself vindicates. Frequently present at negotiations but structurally outvoted or under-resourced relative to Global North negotiating capacity, despite being central to whether global mitigation targets are achievable.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_governments, excluded,
    moderate, generational, constrained, global).

% Produce the physical basis (carbon budgets, warming trajectories) that the mitigation-priority reading treats as authoritative. Do not set policy or collect from the arrangement, but their assessments are cited by every party to justify their position, including parties whose actions diverge sharply from the assessments' implications.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_scientists_and_ipcc, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global reduction in greenhouse gas emissions fast enough to hold warming within a survivable carbon budget, requiring near-simultaneous action across jurisdictions because unilateral restraint without matching action elsewhere fails to change the physical outcome.
% TRANSFER_FUNCTION: Moves capital, employment, and near-term consumption away from fossil-fuel-dependent activity and toward decarbonized infrastructure and toward avoided future climate damages; moves cost disproportionately onto Global North taxpayers and fossil capital now, and moves benefit primarily to future generations and currently climate-vulnerable populations later.
% ABSENT_VOICES: Future generations cannot testify to whether the pace or design of mitigation adequately serves their interests. Fossil-dependent workers are represented in transition-financing rhetoric far more than in transition-financing disbursement. Global South governments are present at negotiations but structurally out-resourced relative to the historical emitters whose burden-sharing they are negotiating against.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority obligation were abandoned overnight, carbon pricing, phase-out mandates, and international climate finance commitments would lose their normative anchor; fossil capital would be substantially re-valued upward as stranding risk receded, decarbonization investment would slow, and the policy architecture built around avoiding future harm would need to be replaced by either an adaptation-only framework or no organized framework at all.
% FOUNDING_PROBLEM: Anthropogenic greenhouse gas emissions were recognized to be committing the planet to warming that would cause severe, difficult-to-reverse harm to ecosystems and human populations, with the harm concentrated in the future and among those with least historical responsibility for causing it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the physical climate science literature (IPCC assessment reports, independent of any government's or industry's policy position) and by actuarial and insurance-industry risk repricing, both of which document accumulating physical risk independent of what any mitigation-priority advocate claims.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.28 to 0.58) as decarbonization policy moved from aspirational target-setting to binding mechanisms with real financial consequences for fossil capital and energy-intensive industry — carbon pricing, stranded-asset risk, and phase-out mandates all impose costs that were largely symbolic in 1992. Suppression rises correspondingly (0.18 to 0.47) as enforcement infrastructure matured: litigation against fossil companies, regulatory phase-outs, disclosure mandates, and financial-sector divestment pressure all represent active mechanisms constraining the exit options of fossil capital and carbon-intensive industry. Theater ratio falls (0.55 to 0.40) as the obligation's machinery became less rhetorical and more consequential, though it remains substantial — considerable diplomatic and corporate activity around climate commitments continues to substitute for binding reduction, particularly at the level of voluntary net-zero pledges without enforcement teeth. Resistance is high (0.72) reflecting active, well-resourced opposition from fossil capital and carbon-intensive industry. Accessibility collapse is moderate (0.42): once the physical logic of carbon budgets is accepted, alternative framings (unlimited fossil use) become difficult to sustain publicly, but genuine competing readings (adaptation-priority, degrowth) remain live and contested, so collapse is far from the near-total closure characteristic of a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and climate-vulnerable nations sit at the beneficiary end of directionality despite bearing no cost — they are structurally powerless but the entire justificatory apparatus of the reading exists on their behalf, which is itself a tension the omega below addresses. Fossil fuel capital sits at the target end: mitigation-priority policy is specifically designed to devalue its core asset base through stranding, and its constrained exit options (cannot simply relocate reserves) sharpen this. Fossil-dependent workers are targets with the least power and the least mobility of any stakeholder, which is why they are listed as payers despite having no decision-making role in the arrangement that costs them their livelihoods. Global North governments occupy an unusual dual position: agenda-setters who administer the obligation, but also bearers of its fiscal and diplomatic costs under the differentiated-responsibility principle the reading itself endorses — this is not a contradiction, it reflects that historical-responsibility framing assigns the administering party disproportionate cost precisely because it administers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accumulating, difficult-to-reverse climate harm concentrated among future and least-responsible populations) remains live by independent physical and actuarial evidence, which is why founding_problem_status is 'live' rather than 'dead' or 'contested' — this distinguishes the mitigation-priority reading from a piton candidate. The classification risk runs the other direction: because enforcement has intensified while theater has only partially declined, there is a live question whether specific mitigation instruments (voluntary net-zero pledges, offset markets) have decoupled from the founding problem and become symbolic compliance layered atop continued emissions — that is a Goodhart-drift risk internal to this reading, tracked by the still-substantial theater_ratio, not evidence against the reading's overall type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_generations_representation_problem,
    'Can an obligation whose primary declared beneficiaries (future generations) cannot participate, consent, or object ever be validated as genuine coordination rather than a proxy claim asserted by currently powerful parties on their behalf?',
    'Compare mitigation policy design and pace against independently modeled optimal-transfer pathways that would actually maximize future welfare; divergence between what is enacted ''for'' future generations and what would actually benefit them under uncontested climate models would indicate the beneficiary claim is partly a cover story for other priorities (e.g., renewable-industry rent-seeking, or Global North governments avoiding harder near-term redistribution).',
    'If policy design tracks modeled future-welfare optimization closely, the tangled_rope reading is well-supported (real coordination for real beneficiaries, with real extraction from fossil capital as the necessary mechanism). If policy design diverges substantially and systematically favors politically convenient measures over welfare-maximizing ones, the extraction share of this reading is understated and closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_representation_problem, conceptual, 'Whether ''future generations as beneficiary'' is a genuine coordination target or a legitimating proxy.').

omega_variable(
    historical_responsibility_burden_calibration,
    'Is the disproportionate mitigation burden placed on the Global North correctly calibrated to historical emissions responsibility, or does it overshoot (extracting more than proportional to causal contribution) or undershoot (allowing continued disproportionate benefit from past emissions while shifting compliance costs onto Global South populations through green-conditionality on finance)?',
    'Comparative accounting of cumulative historical emissions by jurisdiction against actual mitigation-finance flows and domestic decarbonization investment by jurisdiction over the interval.',
    'Undershoot would support classifying Global North governments'' payer role as understated relative to their actual causal responsibility, strengthening the coordination reading. Overshoot or misdirection of committed finance would support treating parts of the arrangement as extraction from Global South conditionality rather than genuine burden-sharing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_responsibility_burden_calibration, empirical, 'Whether differentiated responsibility is honored in practice or is aspirational language layered over underfinanced commitments.').

omega_variable(
    kernel_framing_choice_omega,
    'The kernel''s structural under-determination is visible in the choice among three live readings (mitigation, adaptation, degrowth) that would each authorize a different constraint story with a different ε and different victim/beneficiary sets from the same underlying physical situation. What determines which reading a given policy regime is actually operating under, versus which reading it merely claims rhetorically?',
    'Track whether a jurisdiction''s binding policy instruments (carbon pricing trajectories, phase-out mandates, capital allocation) match the mitigation-priority reading''s implied cost distribution (aggressive near-term fossil-capital devaluation) versus quietly defaulting to adaptation-priority behavior (continued fossil investment with resilience spending) while using mitigation-priority rhetoric.',
    'A jurisdiction found to use mitigation-priority language while its capital allocation matches adaptation-priority behavior would indicate the mitigation_priority constraint, as actually operating there, has a lower real ε and higher theater_ratio than authored here — a signal to decompose further into jurisdiction-specific stories within this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_omega, conceptual, 'How to detect when actual policy has drifted from the mitigation-priority reading it claims, into the adaptation-priority reading in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_obligation__mitigation_priority, theater_ratio, 1992, 0.55).
narrative_ontology:measurement(clim_tr_t1997, climate_response_obligation__mitigation_priority, theater_ratio, 1997, 0.5).
narrative_ontology:measurement(clim_tr_t2005, climate_response_obligation__mitigation_priority, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__mitigation_priority, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__mitigation_priority, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(clim_tr_t2024, climate_response_obligation__mitigation_priority, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_obligation__mitigation_priority, base_extractiveness, 1992, 0.28).
narrative_ontology:measurement(clim_be_t1997, climate_response_obligation__mitigation_priority, base_extractiveness, 1997, 0.33).
narrative_ontology:measurement(clim_be_t2005, climate_response_obligation__mitigation_priority, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__mitigation_priority, base_extractiveness, 2015, 0.47).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__mitigation_priority, base_extractiveness, 2020, 0.53).
narrative_ontology:measurement(clim_be_t2024, climate_response_obligation__mitigation_priority, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_obligation__mitigation_priority, suppression_requirement, 1992, 0.18).
narrative_ontology:measurement(clim_su_t1997, climate_response_obligation__mitigation_priority, suppression_requirement, 1997, 0.22).
narrative_ontology:measurement(clim_su_t2005, climate_response_obligation__mitigation_priority, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__mitigation_priority, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__mitigation_priority, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(clim_su_t2024, climate_response_obligation__mitigation_priority, suppression_requirement, 2024, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, carbon_pricing_mechanism).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_fuel_stranded_asset_risk).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposed from the single natural-language label 'climate response obligation.' The adaptation_priority reading (accept warming, invest in resilience) authors a substantially different beneficiary/victim structure — near-term populations benefit from avoided transition costs, future populations bear the accepted warming as victims — and therefore a different ε profile. The degrowth_reading targets material throughput rather than emissions intensity and implicates continued economic growth itself in the extraction, again with a distinct ε and stakeholder set. All three readings are linked via affects_constraints because policy commitment to one reading materially changes the resource availability and political legitimacy conditions for the others (e.g., strong mitigation investment reduces the perceived urgency and financing available for adaptation infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
