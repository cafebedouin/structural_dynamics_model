% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response Framework
 *   domain: climate_policy/political_economy/intergenerational
 *
 * SUMMARY:
 *   This constraint instantiates the adaptation-priority reading of the
 *   climate-response kernel: the commitment that immediate capital investment
 *   in resilience infrastructure ($540B annually) and acceptance of
 *   temperature rise as inevitable is the legitimate and necessary response
 *   to climate change. The reading prioritizes protection of vulnerable
 *   populations in the present and near-term over reduction of future
 *   warming. It sits in active contest with mitigation-priority (2°C pathway
 *   through emissions cuts + growth) and degrowth-transformation (structural
 *   economic reorganization rejecting growth). The adaptation-priority
 *   reading has become the consensus position in wealthy-nation climate
 *   policy and multilateral climate finance architecture as of ~2015–2020,
 *   effectively foreclosing debate over alternatives in mainstream
 *   institutions. This constraint models that consensus as a structured
 *   extraction mechanism: it solves a genuine near-term coordination problem
 *   (how to protect exposed populations now) while creating asymmetric
 *   burdens on developing nations with limited fiscal capacity and future
 *   generations inheriting higher warming. The claim/metric divergence is
 *   intentional: the constraint is CLAIMED as tangled_rope by its designers
 *   (coordination + some asymmetry justified by urgency), while the authored
 *   metrics show substantial extraction (0.68) sustained by active
 *   enforcement (0.62 suppression) and growing theater (0.41 ratio). The
 *   engine computes the structural reality from these independent facts.
 *
 * KEY AGENTS:
 *   - Wealthy Northern governments (institutional power, agenda-setter): set the international framework and parameters of climate finance; benefit from adaptation framing because it legitimizes continued high-carbon activity
 *   - Developing nations with limited fiscal capacity (moderate power, payer): bear upfront infrastructure costs despite low historical emissions; constrained by negotiating power and need for external financing
 *   - Climate-exposed populations in the Global South (powerless, beneficiary+payer): receive protection selectively based on government investment; carry diffuse cost through higher prices and debt service; cannot negotiate terms
 *   - Infrastructure construction and engineering firms (powerful, beneficiary): capture $100B+ annually in adaptation contracts; invested in promoting adaptation over systemic change
 *   - Climate finance intermediaries (institutional power, beneficiary): manage $100B+ annual flows; benefit from maintaining finance architecture and intermediary roles
 *   - Future generations (powerless, excluded): inherit unmitigated warming ($4–6°C by 2100 under this pathway), shifted risk profiles, and depleted resources; have no voice in decisions but bear the highest cost
 *   - Mitigation-priority advocates (organized, observer+excluded): argue adaptation-priority locks in insufficient reductions and creates false equivalence; structurally marginalized in policy institutions
 *   - Degrowth advocates (organized, excluded): argue adaptation maintains the growth economy that produced the crisis and perpetuates inequality; face framing as unrealistic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.62).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response Framework").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, 'a75e21e5-93d3-462f-99f5-6cf725927940').
narrative_ontology:cs_kernel_codification('a75e21e5-93d3-462f-99f5-6cf725927940', distributed).
narrative_ontology:cs_authority_grounding('a75e21e5-93d3-462f-99f5-6cf725927940', extraction).
narrative_ontology:cs_interpretation_layer_present('a75e21e5-93d3-462f-99f5-6cf725927940').
narrative_ontology:cs_reading_relation('a75e21e5-93d3-462f-99f5-6cf725927940', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('a75e21e5-93d3-462f-99f5-6cf725927940', climate_response_action__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('a75e21e5-93d3-462f-99f5-6cf725927940', foundational, adaptation_mitigation_tradeoff_necessary).
narrative_ontology:cs_axiom_status(adaptation_mitigation_tradeoff_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a75e21e5-93d3-462f-99f5-6cf725927940', adaptation_mitigation_tradeoff_necessary, empirically_contingent).
narrative_ontology:cs_axiom('a75e21e5-93d3-462f-99f5-6cf725927940', foundational, present_protection_over_future_prevention).
narrative_ontology:cs_axiom_status(present_protection_over_future_prevention, holdable).
narrative_ontology:cs_axiom_grounding('a75e21e5-93d3-462f-99f5-6cf725927940', present_protection_over_future_prevention, deontological).
narrative_ontology:cs_reference_frame('a75e21e5-93d3-462f-99f5-6cf725927940', rapid_climate_change_immediate_threat).
narrative_ontology:cs_drift_state('a75e21e5-93d3-462f-99f5-6cf725927940', post_paris_agreement_consensus, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a75e21e5-93d3-462f-99f5-6cf725927940', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, wealthy_northern_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, infrastructure_construction_firms).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, climate_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nations_with_limited_fiscal_capacity).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, climate_exposed_populations_in_global_south).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations_accepting_higher_warming).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, wealthy_northern_governments).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, climate_exposed_populations_global_south).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nations_limited_fiscal_capacity).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, climate_exposed_populations_global_south).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, technological_adaptation_superiority).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, market_based_climate_finance).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, sovereign_carbon_budgets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the international climate policy framework around adaptation investment and climate finance mechanisms. Control the parameters of climate finance (grant/loan splits, conditionality, technology transfer requirements). Benefit from adaptation framing because it legitimizes continued high-carbon economic activity while appearing to address climate harm. Can exit climate agreements entirely or redirect capital allocation; face domestic political pressure to act but retain structural power over resource flows.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, wealthy_northern_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, wealthy_northern_governments, beneficiary).

% Bear the upfront capital costs of resilience infrastructure ($540B annually distributed globally, with unequal burden-sharing) despite producing 10% of cumulative emissions. Must mobilize domestic resources or accept debt-financed adaptation, deepening fiscal vulnerability. Cannot unilaterally exit adaptation requirement (climate impacts are non-negotiable); face structural pressure to accept whatever financing terms Northern nations offer. Limited bargaining power in international climate negotiations.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nations_limited_fiscal_capacity, payer,
    moderate, generational, constrained, global).

% Receive protection from climate impacts through adaptation infrastructure (flood barriers, drought-resistant crops, early warning systems) but only where wealthy nations or their own governments prioritize investment. Bear costs through higher prices for infrastructure-dependent goods, debt service on borrowed financing, and displacement from protected lands. Cannot negotiate for better protection; benefit is unevenly distributed based on geography and wealth. Climate migration is constrained by national borders.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_exposed_populations_global_south, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, climate_exposed_populations_global_south, payer).

% Capture substantial contracts from climate adaptation spending ($100B+ annually in construction and engineering). Benefit from adaptation framing because it mandates physical infrastructure (dams, levees, irrigation systems, buildings, transportation networks) rather than systemic economic change. Can relocate operations and compete globally for adaptation contracts. Invest in lobbying to frame adaptation as the primary climate response pathway.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, infrastructure_construction_firms, beneficiary,
    powerful, biographical, mobile, global).

% Manage and administer climate finance flows ($100B+ annually through multilateral development banks, green funds, bilateral agreements). Capture origination fees, management fees, and administrative overhead. Benefit from adaptation framing because it maintains finance architecture and intermediary roles; alternative framings (degrowth, mitigation-only) would disintermediate or require different institutional structures. Can shift between climate funds; face regulatory pressure to demonstrate climate impact.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_finance_intermediaries, beneficiary,
    institutional, generational, arbitrage, global).

% Cannot participate in present decisions but inherit the outcome: adaptation infrastructure in some regions, unmitigated warming ($4–6°C by 2100 under this pathway), shifted risk profiles, and depleted resources used for protection rather than systemic decarbonization. The adaptation-priority reading explicitly accepts higher future warming costs as the trade-off for protecting present populations in wealthy nations. Have no voice in negotiations or consent mechanisms.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Argue that adaptation-priority framing locks in insufficient emissions reductions and produces false equivalence between adaptation and mitigation. Contend that accepting higher warming makes adaptation itself increasingly futile and that capital committed to adaptation could instead fund decarbonization. Face institutional barriers to reframing (adaptation is now policy consensus in most wealthy governments); must work within existing climate governance structures.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, mitigation_priority_advocates, observer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, mitigation_priority_advocates, excluded).

% Argue adaptation-priority maintains the growth-dependent economy that produced the climate crisis and perpetuates inequality. Contend that protecting wealthy populations through adaptation while accepting warming harms the Global South and future generations. Structurally excluded from major climate policy institutions; face framing as unrealistic or radical; constrained to advocacy and small-scale alternative practice.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, degrowth_advocates, excluded,
    organized, generational, constrained, global).

% Documents the constraint's structure, beneficiary distribution, and alignment with equity principles. Notes the financing gap ($350B annually) between wealthy-nation commitments and developing-nation needs. Observes that adaptation-priority reading is internally coherent but produces substantial extraction from fiscally constrained nations and future generations. Cannot directly influence policy but can inform constituency awareness and feed evidence to negotiations.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__adaptation_priority, wealthy_northern_governments).
narrative_ontology:fixing_cost_class(climate_response_action__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes capital mobilization and deployment for climate resilience infrastructure at scale, solving the collective-action problem of unprofitable adaptation investment by routing financing through public and blended-finance mechanisms. Coordinates risk-sharing across national borders and timeframes (wealthier nations accepting some fiscal liability for adaptation in poorer nations). Creates information standards for climate risk assessment and infrastructure design specifications.
% TRANSFER_FUNCTION: Moves $540B annually in adaptation financing from public budgets and private capital markets primarily in wealthy nations to construction, engineering, and finance firms, with a portion flowing to vulnerable-population protection in the Global South. Moves fiscal risk from wealthier nations (absorbing adaptation costs as infrastructure investment) to developing nations (taking on debt or internal reallocation to fund adaptation). Moves decision authority over adaptation priorities from affected populations to international technocrats and Northern governments.
% ABSENT_VOICES: Developing nations with limited negotiating power are present but subordinate; future generations and non-human systems have no direct voice. Mitigation-priority advocates and degrowth advocates are structurally excluded from shaping the framework (they lost the policy contest in ~2015–2020); they would argue for radically different resource allocation and that adaptation-priority legitimizes inadequate climate action.
% DISAPPEARANCE_RATIONALE: If adaptation-priority framing and its financing architecture vanished, climate policy would revert to mitigation-only or degrowth framings, redirecting $540B annual capital from infrastructure to decarbonization and economic restructuring; risk distribution would shift from public-finance solutions to private-insurance and individual-protection models, leaving uninsured populations exposed; international climate governance would reorganize around emissions-reduction targets rather than adaptation finance flows.
% FOUNDING_PROBLEM: Rapid climate change produces immediate threats to vulnerable populations (flooding, drought, heat waves) that cannot wait for decarbonization to be completed (50+ year process); adaptation infrastructure can reduce mortality and economic loss in the near term (5–30 years) while mitigation runs its course. Early 2010s realized that 2°C warming is no longer avoidable given historical emissions and political constraints on rapid decarbonization; protecting people through adaptation became the practical survival question.
% FOUNDING_PROBLEM_CORROBORATION: IPCC climate scientists and adaptation-priority governments attest the founding problem is live and urgent. Mitigation advocates and Global South economists contest the problem statement: they argue the founding problem should be 'how to prevent catastrophic warming' not 'how to protect present populations under high warming,' and that accepting high warming as inevitable is a self-fulfilling prophecy that reduces political will for emissions reductions. Corroboration from outside the benefiting parties: small island state negotiators and climate justice advocates explicitly reject the founding problem as stated; they argue it privileges Northern adaptation over Southern emissions reductions and makes the adaptation-priority reading a constructed solution to a constructed problem.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily from 0.48 (t=0) to 0.68 (t=50) because adaptation-priority financing concentrates gains in Northern governments, construction firms, and financial intermediaries while diffusing costs across developing nations and future generations. The $350B financing gap between committed funds and estimated need is the structural indicator: wealthy nations commit ~$100B annually in adaptation finance but developing nations need ~$450B, creating permanent fiscal subordination. Suppression rises from 0.45 to 0.62 because active enforcement is required to maintain this reading's dominance: exclusion of mitigation-only and degrowth framings from policy institutions, dismissal of Global South arguments for deeper emissions cuts, suppression of future-generation interests through intergenerational decision structures that don't include them. Theater ratio starts lower (0.25) and rises to 0.42 because the coordination function (protecting vulnerable populations) is real early but increasingly becomes cover for rent-seeking (construction contracts, finance fees) as the constraint matures. The coercion grid shows asymmetric pressure: structural-level pressure from climate impacts and finance obligations is severe and rising (stakes_inflation 0.68→0.85); organizational-level resistance is strong (0.72→0.78) from competing climate framings; class-level resistance declines (0.75→0.72) as fatigue sets in and alternative framings are normalized as unrealistic. Individual-level resistance also declines (0.68→0.65) as adaptation becomes taken-for-granted inevitability and people internalize the frame. The grid asymmetry is the mechanism: structural weight increases while grassroots capacity to resist decreases.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy-Northern-government agenda-setter seat, this is a necessary coordination mechanism solving the collective-action problem of unprofitable adaptation investment; the beneficiary/payer distinction is justified by efficiency and urgency. From the developing-nation payer seat, the same structure appears as imposed fiscal obligation backed by unequal power and limited negotiating room. From the climate-exposed-population seat, adaptation benefits are unevenly distributed (protection correlates with wealth and geography) and come with diffuse costs (debt, displacement, price inflation). From the future-generation seat (excluded), this is a choice to accept $4–6°C warming in exchange for present-population protection—a trade that those making it do not bear the long-term cost of. The engine's per-seat computation should show these divergences: powerful agenda-setters computing a rope (coordination justified by asymmetry); constrained payers computing snare (coerced participation in a mechanism that extracts from them); excluded voices computing pure extraction (their interests are not represented in the coordination problem statement itself).
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy Northern governments are near the full beneficiary end (d ≈ 0.15): they set the rules, control financing allocation, legitimize continued high-carbon activity, and shift adaptation costs to others while capturing benefits of infrastructure investment in their own territories. Infrastructure firms and finance intermediaries are beneficiaries (d ≈ 0.20): they collect contracts and fees without bearing implementation risk. Developing nations are targets (d ≈ 0.75): they must mobilize capital they don't have, accept financing terms they didn't set, and bear the asymmetric fiscal burden of protecting themselves from climate impacts they didn't cause. Climate-exposed populations sit asymmetrically (d ≈ 0.65 for those in developing nations, d ≈ 0.30 for those in wealthy nations): those in the Global South depend on government investment they cannot control; those in wealthy nations benefit from adaptation infrastructure and lower climate risk. Future generations are pure targets (d ≈ 1.0): they inherit the outcome (high warming) without having participated in the trade-off decision. The directionality derivation flows from beneficiary/victim declarations + exit options: Northern governments have arbitrage-grade exit (can withdraw from climate agreements), wealthy-nation populations have mobile exit (can invest privately in protection), developing nations have constrained exit (adaptation is non-negotiable, financing terms are imposed), climate-exposed populations have trapped exit (geographic immobility, climate impacts are non-negotiable), future generations have no exit. This structural asymmetry should produce clear per-seat type divergence: rope/tangled_rope from powerful seats, snare from constrained payers, pure extraction from excluded voices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rapid climate change produces immediate threats; decarbonization is slow; adaptation can reduce near-term mortality) is live and urgent. However, mandatrophy analysis reveals a secondary question: is acceptance of 4–6°C warming as inevitable the only defensible response, or does adaptation-priority framing pre-foreclose faster decarbonization pathways? Mitigation advocates argue the founding problem should be framed as 'how to prevent unmitigable warming' rather than 'how to protect people under high warming,' and that the adaptation-priority reading manufactures inevitability through self-fulfilling prophecy (if political will for emissions reduction is perceived as futile, politicians stop trying). From this perspective, adaptation-priority resolves the immediate survival question while creating long-term mandatrophy: the coordination function (protection now) persists, but the founding problem (prevent catastrophic warming) atrophies and is abandoned. The measurement series shows extractiveness rising toward an asymptote (0.68 by t=50), suggesting the constraint reaches a steady-state extraction level where the adaptation apparatus is mature and captures its maximum rent. If true, this indicates mandatrophy is already present at t=50: the coordination function is established, the extraction function is maximized, but the founding problem (preventing warming) is no longer the organizing principle—present-population protection is. The analysis of whether the constraint has resolved or abandoned its founding problem depends on which reading of the kernel you adopt: adaptation-priority says the founding problem has been solved (protection is now possible); mitigation-priority says the founding problem has been abandoned (warming prevention is no longer the goal).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    warming_inevitability_framing,
    'Is 4–6°C warming genuinely inevitable given current political and technological constraints, or is it inevitable only if adaptation-priority framing pre-forecloses faster decarbonization?',
    'Counterfactual analysis comparing political will for emissions reduction under adaptation-priority vs. alternative framings; historical analysis of when the ''inevitability'' of high warming became accepted in policy discourse; modeling of decarbonization pathways that would require different framing to achieve.',
    'If warming is politically contingent on the choice of framing, adaptation-priority is a self-fulfilling prophecy that creates the very outcome it claims to accept. If warming is inevitable despite alternative framings, adaptation-priority is the appropriate response. This distinction determines whether the constraint is mandatrophic (abandoning the founding problem) or solving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_inevitability_framing, conceptual, 'Whether high warming is inevitable or contingent on adoption of adaptation-priority framing.').

omega_variable(
    financing_gap_source,
    'Is the $350B annual financing gap the result of developing nations'' limited fiscal capacity (structural constraint) or wealthy nations'' under-commitment to the adaptation-priority framing they advocate?',
    'Analysis of historical climate finance pledges vs. actual disbursement; comparison of adaptation-finance spending as percentage of wealthy-nation GDP vs. domestic climate spending; modeling of what financing gap would close with different redistributive mechanisms.',
    'If the gap is structural (developing nations genuinely cannot mobilize needed capital), adaptation-priority is salvageable only through massive wealth redistribution that Northern governments resist. If the gap is artificial (wealthy nations under-commit while framing it as inevitable), the constraint''s extraction function is transparent. Either way, perpetual inequality in adaptation investment is built into the current architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financing_gap_source, empirical, 'Whether the financing gap is structural or policy-contingent.').

omega_variable(
    intergenerational_trade_acceptance,
    'Is explicit acceptance of higher future warming (4–6°C instead of 2°C) to enable present-population protection ethically defensible, or does it violate intergenerational equity principles?',
    'Normative analysis from intergenerational justice frameworks; empirical analysis of whether future-generation costs (unmitigated warming impacts, depleted adaptation-investment resources, shifted risk profiles) exceed present-generation benefits; assessment of whether future generations have any decision-making role in the trade-off.',
    'If future-generation exclusion is ethically unjustifiable, the constraint may require restructuring to include intergenerational representation or to reject the high-warming trade-off. If the trade-off is ethically defensible, adaptation-priority remains justified. This determination affects whether the constraint is extractive or coordinating from the intergenerational perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_trade_acceptance, preference, 'Whether explicit acceptance of higher future warming is ethically defensible.').

omega_variable(
    adaptation_saturation_point,
    'Is there a physical limit to how much adaptation can protect populations from climate impacts, and if so, at what warming threshold does adaptation become insufficient?',
    'Climate impact modeling showing adaptation effectiveness at different warming levels (1.5°C, 2°C, 3°C, 4°C, 6°C); failure-mode analysis of adaptation infrastructure under compound climate impacts; historical analysis of adaptation limits in past climate-change events.',
    'If adaptation approaches saturation before 4–6°C warming, the constraint''s promise to protect vulnerable populations is false even at its own success metrics. If adaptation can extend protection to higher warming levels, the constraint remains viable. This affects the truthfulness of the agenda-setter''s coordination narrative and whether adaptation-priority is a coherent strategy or rationalization for inaction on mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_saturation_point, empirical, 'Whether adaptation can protect populations at 4–6°C warming or has saturation limits.').

omega_variable(
    kernel_reading_alternative_foreclosure,
    'Does adoption of adaptation-priority reading foreclose mitigation-priority or degrowth-transformation readings, or do all three coexist as live options held by different parties?',
    'Mapping of which international institutions, nation-states, and advocacy coalitions hold each reading; analysis of whether parties holding adaptation-priority actively suppress or merely disagree with alternative readings; assessment of whether institutional structures allow all three readings to be heard or whether adaptation-priority has achieved hegemony.',
    'If adaptation-priority forecloses the other readings (they become logically incoherent or institutionally impossible), the kernel contest is resolved and the alternative readings are not true alternatives. If the readings coexist, the kernel remains genuinely contested and the constraint story is one reading among live competitors. This affects how the engine should classify the constraint''s type per seat: from mitigation-advocates'' perspective, adaptation-priority is a false-summit constraint (claims to address climate response while abandoning emissions reduction); from adaptation-advocates'' perspective, it is genuine coordination (best available response to inevitable warming).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_foreclosure, empirical, 'Whether adaptation-priority forecloses or coexists with alternative climate-response readings.').

omega_variable(
    suppression_mechanism_externalization,
    'Is the measured suppression of alternative readings (mitigation-priority, degrowth) structural (alternative readings genuinely have no institutional power) or internalized (advocates of alternatives have internalized adaptation-priority as inevitable and stopped articulating their own framings)?',
    'Longitudinal analysis of climate policy discourse: are mitigation-priority and degrowth arguments disappearing from mainstream institutions (suppressed) or being actively articulated but losing debates (contested)? Post-suppression trajectory: if adaptation-priority framework were removed, would alternative framings reemerge or remain dormant? Survey data on whether climate advocates believe alternative readings are impossible or merely politically disfavored.',
    'If suppression is structural (alternatives are institutionally locked out), the constraint is sustained by coercion and abandoning the suppression apparatus would destabilize it. If suppression is internalized (advocates believe alternative readings are impossible), the constraint may be self-sustaining through belief even if external coercion is reduced. This affects the cost of removing or restructuring the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_externalization, empirical, 'Whether suppression of alternative climate-response readings is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_response_action__adaptation_priority, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_action__adaptation_priority, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__adaptation_priority, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t25, climate_response_action__adaptation_priority, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t35, climate_response_action__adaptation_priority, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(clim_tr_t35, projected).
narrative_ontology:measurement(clim_tr_t50, climate_response_action__adaptation_priority, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(clim_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_response_action__adaptation_priority, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_action__adaptation_priority, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_action__adaptation_priority, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t25, climate_response_action__adaptation_priority, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t35, climate_response_action__adaptation_priority, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(clim_be_t35, projected).
narrative_ontology:measurement(clim_be_t50, climate_response_action__adaptation_priority, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(clim_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_response_action__adaptation_priority, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_action__adaptation_priority, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_action__adaptation_priority, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t25, climate_response_action__adaptation_priority, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t35, climate_response_action__adaptation_priority, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(clim_su_t35, projected).
narrative_ontology:measurement(clim_su_t50, climate_response_action__adaptation_priority, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(clim_su_t50, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(clim_grid_01, climate_response_action__adaptation_priority, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(clim_grid_02, climate_response_action__adaptation_priority, accessibility_collapse(class), 50, 0.44).
narrative_ontology:measurement(clim_grid_03, climate_response_action__adaptation_priority, accessibility_collapse(individual), 0, 0.38).
narrative_ontology:measurement(clim_grid_04, climate_response_action__adaptation_priority, accessibility_collapse(individual), 50, 0.48).
narrative_ontology:measurement(clim_grid_05, climate_response_action__adaptation_priority, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(clim_grid_06, climate_response_action__adaptation_priority, accessibility_collapse(organizational), 50, 0.62).
narrative_ontology:measurement(clim_grid_07, climate_response_action__adaptation_priority, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(clim_grid_08, climate_response_action__adaptation_priority, accessibility_collapse(structural), 50, 0.78).
narrative_ontology:measurement(clim_grid_09, climate_response_action__adaptation_priority, resistance(class), 0, 0.75).
narrative_ontology:measurement(clim_grid_10, climate_response_action__adaptation_priority, resistance(class), 50, 0.72).
narrative_ontology:measurement(clim_grid_11, climate_response_action__adaptation_priority, resistance(individual), 0, 0.68).
narrative_ontology:measurement(clim_grid_12, climate_response_action__adaptation_priority, resistance(individual), 50, 0.65).
narrative_ontology:measurement(clim_grid_13, climate_response_action__adaptation_priority, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(clim_grid_14, climate_response_action__adaptation_priority, resistance(organizational), 50, 0.78).
narrative_ontology:measurement(clim_grid_15, climate_response_action__adaptation_priority, resistance(structural), 0, 0.62).
narrative_ontology:measurement(clim_grid_16, climate_response_action__adaptation_priority, resistance(structural), 50, 0.68).
narrative_ontology:measurement(clim_grid_17, climate_response_action__adaptation_priority, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement(clim_grid_18, climate_response_action__adaptation_priority, stakes_inflation(class), 50, 0.72).
narrative_ontology:measurement(clim_grid_19, climate_response_action__adaptation_priority, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_20, climate_response_action__adaptation_priority, stakes_inflation(individual), 50, 0.68).
narrative_ontology:measurement(clim_grid_21, climate_response_action__adaptation_priority, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(clim_grid_22, climate_response_action__adaptation_priority, stakes_inflation(organizational), 50, 0.75).
narrative_ontology:measurement(clim_grid_23, climate_response_action__adaptation_priority, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(clim_grid_24, climate_response_action__adaptation_priority, stakes_inflation(structural), 50, 0.85).
narrative_ontology:measurement(clim_grid_25, climate_response_action__adaptation_priority, suppression(class), 0, 0.35).
narrative_ontology:measurement(clim_grid_26, climate_response_action__adaptation_priority, suppression(class), 50, 0.42).
narrative_ontology:measurement(clim_grid_27, climate_response_action__adaptation_priority, suppression(individual), 0, 0.32).
narrative_ontology:measurement(clim_grid_28, climate_response_action__adaptation_priority, suppression(individual), 50, 0.38).
narrative_ontology:measurement(clim_grid_29, climate_response_action__adaptation_priority, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(clim_grid_30, climate_response_action__adaptation_priority, suppression(organizational), 50, 0.58).
narrative_ontology:measurement(clim_grid_31, climate_response_action__adaptation_priority, suppression(structural), 0, 0.58).
narrative_ontology:measurement(clim_grid_32, climate_response_action__adaptation_priority, suppression(structural), 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, intergenerational_risk_transfer).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, north_south_economic_inequality).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the climate_response_action kernel. The sibling readings (mitigation_priority, degrowth_transformation) are separate constraint stories with different epsilon values, beneficiary structures, and claim/metric profiles. All three readings solve a genuine coordination problem (how to respond to climate change) but via incompatible mechanisms and with divergent distributional consequences. They form a constraint family linked by network.affects_constraints; each story models one reading as a standalone, epsilon-invariant constraint. The kernel contest is the fact that all three are live options held by different parties; no single reading determines which is 'correct.' The adaptation-priority reading (this constraint) has achieved consensus in mainstream institutions but remains contested by mitigation advocates and degrowth advocates at lower institutional power levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__adaptation_priority, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
