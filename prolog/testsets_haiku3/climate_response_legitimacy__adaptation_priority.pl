% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of climate response legitimacy frames
 *   warming as inevitable and shifts the ethical burden from preventing
 *   emissions to building resilience in vulnerable regions. Wealthy nations,
 *   financial gatekeepers, and technology vendors benefit by preserving
 *   high-carbon development models while controlling adaptation finance.
 *   Low-income regions, bearing immediate warming impacts and adaptation
 *   deficits, become locked-in payers. The reading operates as a tangled
 *   rope: genuine coordination problem (how to help vulnerable populations
 *   survive warming) fused with asymmetric extraction (preservation of
 *   wealthy-nation development rights, deferral of emissions reduction,
 *   intergenerational cost-shifting). The measurement series track rising
 *   extractiveness as impacts accumulate and adaptation spending fails to
 *   keep pace; rising theater as adaptation performance (climate finance
 *   announcements, resilience rhetoric) substitutes for emissions reduction;
 *   rising suppression as enforcement of the adaptation-priority framing
 *   crowds out mitigation and degrowth alternatives.
 *
 * KEY AGENTS:
 *   - wealthy_nations_development_model: preserves high-carbon growth; controls adaptation finance agenda
 *   - low_income_vulnerable_regions: trapped by geography and economics; face compounded warming impacts and adaptation deficits
 *   - adaptation_finance_gatekeepers: institutional; allocate and extract returns from adaptation capital
 *   - future_generations_high_warming: non-agent; inherit deferred emissions and higher baseline warming
 *   - mitigation_technology_vendors: powerful; benefit from proprietary technology in adaptation spending
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.71).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Climate Response Framework").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '9f0f8c50-f5c7-4773-a850-777468cfb8e7').
narrative_ontology:cs_kernel_codification('9f0f8c50-f5c7-4773-a850-777468cfb8e7', distributed).
narrative_ontology:cs_authority_grounding('9f0f8c50-f5c7-4773-a850-777468cfb8e7', extraction).
narrative_ontology:cs_interpretation_layer_present('9f0f8c50-f5c7-4773-a850-777468cfb8e7').
narrative_ontology:cs_reading_relation('9f0f8c50-f5c7-4773-a850-777468cfb8e7', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('9f0f8c50-f5c7-4773-a850-777468cfb8e7', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('9f0f8c50-f5c7-4773-a850-777468cfb8e7', foundational, warming_acceptance_foundational).
narrative_ontology:cs_axiom_status(warming_acceptance_foundational, holdable).
narrative_ontology:cs_axiom_grounding('9f0f8c50-f5c7-4773-a850-777468cfb8e7', warming_acceptance_foundational, empirically_contingent).
narrative_ontology:cs_axiom('9f0f8c50-f5c7-4773-a850-777468cfb8e7', foundational, adaptation_prioritization_legitimate).
narrative_ontology:cs_axiom_status(adaptation_prioritization_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9f0f8c50-f5c7-4773-a850-777468cfb8e7', adaptation_prioritization_legitimate, instrumental).
narrative_ontology:cs_reference_frame('9f0f8c50-f5c7-4773-a850-777468cfb8e7', pragmatic_inevitability).
narrative_ontology:cs_drift_state('9f0f8c50-f5c7-4773-a850-777468cfb8e7', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9f0f8c50-f5c7-4773-a850-777468cfb8e7', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations_development_model).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_finance_gatekeepers).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations_high_warming).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, mitigation_technology_vendors).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, wealthy_small_island_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wealthy economies frame adaptation as the primary climate response path, justifying continued high-carbon development trajectories in the present on the grounds that future adaptive capacity (funded through growth revenues) will handle impacts. This preserves industrial, energy-intensive, consumption-based development models in high-income nations while shifting the burden of living with warming to lower-income regions. Frames the choice as pragmatic and inevitable rather than extractive.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations_development_model, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, wealthy_nations_development_model, agenda_setter).

% Face the compounded burden of living with warming in the near term (agricultural collapse, water stress, heat mortality, climate migration) while bearing the cost of adaptation infrastructure they cannot afford ($350B annual gap documented by UNEP). Trapped by geography, economic dependence on global supply chains controlled by wealthy nations, and lack of domestic capital for resilience investment. Their exit from this arrangement (unilateral emissions reduction without wealthy-nation support) produces no material benefit and worsens their relative position.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions, payer,
    powerless, biographical, trapped, regional).

% Multilateral development banks, wealthy-nation climate finance institutions, and private equity funds that control and allocate adaptation capital. They enforce the adaptation-priority framing by conditioning climate finance on technology adoption, governance reforms, and market-opening reforms that benefit wealthy-nation investors. Set eligibility criteria, disburse funds, and extract fees and returns; preserve the existing development model by making adaptation compatible with it.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_finance_gatekeepers, agenda_setter,
    institutional, generational, mobile, global).

% A non-agent collective facing compounded warming impacts: the adaptation-priority reading defers emissions reduction, accepting higher cumulative warming by mid-century. Each decade of continued high-emissions development locks in additional warming that adaptation infrastructure in 2070 cannot fully offset. They inherit the doubled-down extraction: both the immediate impacts of higher warming AND the costs of adaptation to that higher baseline.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations_high_warming, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__adaptation_priority, future_generations_high_warming).

% Vendors of renewable energy, smart grid technology, electric vehicles, carbon capture, and climate-resilient agriculture benefit from adaptation framing because adaptation spending on drought-resistant crops, flood-resistant infrastructure, and climate-monitoring systems often incorporates proprietary technology requiring ongoing licensing or purchase. Have structural incentive to support adaptation-priority framing as it expands their addressable market.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, mitigation_technology_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Face existential climate risk (submersion) that cannot be adapted away through infrastructure alone; adaptation spending buys time but does not solve the underlying problem. Trapped between accepting adaptation framing (which leaves their islands submerged) and demanding higher mitigation targets (which wealthy nations resist). Their exit is literal: potential full migration, which wealthy nations have not committed to facilitating.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_small_island_states, payer,
    moderate, generational, constrained, local).

% Negotiate UNFCCC provisions, operationalize climate finance, and set emissions reduction targets. Observe and often mediate between wealthy-nation preference for adaptation framing and low-income-nation demands for emissions reduction. Their observations shape whether the constraint is characterized as inevitable trade-off or as distributionally extractive.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, international_climate_negotiators, observer,
    institutional, generational, analytical, global).

% Represent centuries of low-carbon, adaptive resource management in vulnerable regions but are structurally excluded from adaptation-finance decision-making, which privileges technologically engineered solutions and formal institutional channels over traditional ecological knowledge. Would object to adaptation framing as imposed modernization rather than legitimate resilience-building if their voices were in the room.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, indigenous_knowledge_systems, excluded,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__adaptation_priority, indigenous_knowledge_systems).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, adaptation_finance_gatekeepers).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate global response to climate warming: establish legitimacy framework for distinguishing necessary from extractive climate action; allocate responsibility for emissions reduction vs. adaptation investment; create funding mechanisms to help vulnerable populations survive warming impacts.
% TRANSFER_FUNCTION: Transfers near-term development rights (continued high-carbon growth) from wealthy nations to adaptation costs borne by low-income regions. Wealthy economies preserve current development model and extract returns on adaptation investments; low-income regions pay through climate impacts, infrastructure deficits, and debt-financed adaptation. Intergenerational transfer: defers emissions reduction, locking in higher cumulative warming that future generations must adapt to or endure.
% ABSENT_VOICES: Indigenous knowledge systems, subsistence-economy populations, and climate-displaced communities are excluded from adaptation-finance decision-making. They would frame adaptation as imposed technological modernization rather than cultural resilience, and would demand emissions reduction as the primary path. Mitigation-priority and degrowth-transformation advocates are also excluded from wealthy-nation climate policy cores where adaptation framing is operationalized.
% DISAPPEARANCE_RATIONALE: If adaptation-priority framing and its enforcement machinery vanished, wealthy nations would face immediate pressure to commit to deeper emissions reductions (higher mitigation costs imposed on them). Low-income regions would reorient toward emissions reduction support and technology transfer rather than debt-financed adaptation. Climate finance would shift from adaptation infrastructure (captured by technology vendors and gatekeepers) toward mitigation and structural transformation. The global development model would be contested rather than preserved.
% FOUNDING_PROBLEM: Late 1990s–2010s: wealthy nations recognized climate change required response but faced cost of rapid decarbonization (industrial transformation, energy system overhaul, consumption reduction). Adaptation framing emerged as solution: acknowledge warming will occur, shift responsibility to managing impacts rather than preventing them, preserve economic growth model while building resilience. Presented as realistic constraint-acceptance rather than structural choice.
% FOUNDING_PROBLEM_CORROBORATION: Wealthy-nation climate economists and policy institutions attest the founding problem remains live: decarbonization is too costly, so adaptation is necessary. Low-income nations and climate-displaced communities attest the founding problem is a false framing—the real problem is wealthy-nation resistance to emissions reduction, which adaptation finance obscures rather than solves. Independent analysis by IPCC, UNEP, and post-Paris climate finance assessments corroborates that adaptation spending is inadequate relative to impacts and that emissions reduction deferrals compound future costs—supporting the contested status.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (2010, early adaptation framing) to 0.74 (2050, high-warming adaptation-locked pathway) because each decade of deferred emissions reduction compounds the adaptation deficit. The $350B annual gap between adaptation needs and finance represents direct structural extraction from low-income regions. Theater rises from 0.18 to 0.45 as adaptation-finance announcements, resilience narratives, and climate-resilient agriculture initiatives become performative substitutes for emissions reduction. Suppression rises from 0.48 to 0.73 as enforcement machinery (climate-finance conditionality, technology vendor lock-in, exclusion of degrowth and mitigation-priority voices from negotiation rooms) hardens. The coercion grid shows individual-level stakes inflation rising steeply (0.28 → 0.82): households in vulnerable regions face both warming impacts and debt servicing for adaptation. Organizational-level resistance stays high (0.62 → 0.73) because civil-society organizations and low-income-nation governments continuously contest the framing, but the constraint's enforcement machinery (gatekeeper control, multilateral-institution dominance) prevents their resistance from shifting the path.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy-nation and gatekeeper seats, the adaptation-priority reading appears pragmatic and inevitable: costly emissions reduction is deferred, but building resilience now protects vulnerable populations immediately. From the low-income-region and future-generation seats, the same constraint operates as imposed extraction: they pay the cost of wealthy-nation decarbonization deferral through both immediate impacts and compounded future warming. The engine should compute tangled_rope from the weighted seats because coordination (helping vulnerable populations survive) is genuine and one beneficiary exists (wealthy development models), but asymmetric extraction is equally real (low-income victims, deferred mitigation costs). The claimed type and metric profile are independent: the claim asserts rope-like coordination; the metrics describe substantially extractive operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy-nation development model: role=beneficiary + agenda_setter, d ≈ 0.15 (benefits from growth preservation, sets the framing, controls exit). Adaptation gatekeepers: role=agenda_setter, d ≈ 0.20 (controls capital flows and conditions, benefits from extraction). Low-income regions: role=payer, d ≈ 0.88 (trapped by geography, faces impacts and deficits, no effective exit). Future generations: role=payer (non-agent), d ≈ 0.92 (completely trapped; inherit compounded warming). No directionality overrides are needed—the derivation from beneficiary/victim + exit_options produces accurate seats. Coalition power is low for powerless agents: low-income regions are individually weak but organized (national governments, civil society) at the class level, which is where resistance emerges (0.68 at t0, 0.75 at tn). Their resistance does not translate to exit because the constraint is enforced by structures they cannot leave (global climate system, debt obligations to gatekeepers).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is contested: wealthy nations claim the problem is 'how do we afford deep decarbonization while helping vulnerable regions adapt' (where adaptation-priority is the answer); low-income nations claim the founding problem is 'how do wealthy nations avoid decarbonization costs while appearing to address climate change' (where adaptation-priority is the betrayal). The disappearance verdict (world_rearranges) indicates the constraint is constructed, not inevitable. Mandatrophy is NOT resolved: the founding problem—whether adaptation-first or emissions-reduction-first is legitimate—remains live and contested. The engine should flag this as a constraint whose mandate (what problem it solves) is under active contestation, not as a degraded institutional form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_sufficiency_frontier,
    'Is there a warming magnitude beyond which adaptation becomes physically or economically infeasible, rendering the adaptation-priority reading obsolete?',
    'IPCC impact assessments at cumulative warming thresholds (3°C, 4°C, 5°C+); economic modeling of adaptation cost curves relative to GDP in vulnerable regions; empirical observation of climate-induced state collapse or mass migration.',
    'If adaptation becomes infeasible above ~2.5°C, the reading''s foundational premise (warming is manageable through resilience) collapses, and the constraint reclassifies from tangled_rope toward snare (pure extraction with no coordination outcome). If adaptation remains feasible to 4°C+, the reading''s viability extends and the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_frontier, empirical, 'Whether adaptation infrastructure can meaningfully offset impacts across plausible warming scenarios.').

omega_variable(
    finance_adequacy_vs_extraction_definition,
    'If climate finance to low-income regions met the $350B annual gap and achieved effective adaptation outcomes, does the constraint cease to be extractive, or does the extraction lie in the deferral of emissions reduction itself (independent of adaptation adequacy)?',
    'Historical test: if adaptation finance reaches $350B+/year and vulnerable-region climate resilience improves materially, does their measured extraction decline to match wealthy-nation levels? If extraction persists despite adequacy, the extraction is the emissions-reduction deferral itself, not the finance gap.',
    'If extraction is finance-dependent: tangled_rope persists as long as finance is inadequate; reclassifies toward rope if finance becomes adequate. If extraction is deferral-dependent: tangled_rope persists regardless of finance level (the cost of high-warming adaptation is compounded regardless of financing source). The resolution determines whether the constraint is fixable through redistribution or requires structural transformation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(finance_adequacy_vs_extraction_definition, conceptual, 'Whether the constraint''s extractiveness is rooted in finance inequality or in the deferral of emissions reduction itself.').

omega_variable(
    intergenerational_cost_compounding,
    'How much of the measured extractiveness on future generations is structural (higher cumulative warming = larger adaptation burden) vs. institutional (decisions made today to preserve growth lock in future paths)?',
    'Comparison of two counterfactual pathways: (1) adaptation-priority path with observed emissions deferral (status quo), (2) early-mitigation path with equivalent adaptation spending but lower cumulative warming. The difference in future-generation burden is the compounding cost of deferral.',
    'If compounding cost is substantial (>30% of total future burden), the constraint''s extraction of future generations is a primary structural feature and should be highlighted as intergenerational injustice, not merely an unfortunate trade-off. If compounding is minor, adaptation-priority is more defensible as a real coordination solution despite deferral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_compounding, empirical, 'The magnitude of cumulative warming cost imposed on future generations by this reading''s deferral of emissions reduction.').

omega_variable(
    excluded_voice_power_asymmetry,
    'Would indigenous knowledge systems and subsistence-economy communities, if included in adaptation-finance decision-making as equals, advocate for different adaptation strategies than technology vendors and multilateral institutions currently propose?',
    'Participatory research in adaptation-finance jurisdictions where indigenous decision-making authority is recognized; comparison of outcomes between community-led and institution-led adaptation projects.',
    'If excluded voices advocate substantially different strategies (e.g., landscape restoration over engineered infrastructure, traditional water management over mega-dams), their exclusion is enforcing a particular adaptation model, and the suppression metric understates the true suppression of alternatives. The constraint shifts from tangled_rope (coordination + asymmetric extraction) toward snare (suppression-dependent extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(excluded_voice_power_asymmetry, empirical, 'Whether adaptation-finance exclusion mechanisms enforce technology-vendor and gatekeeper preferences over community preferences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 2010, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2010, climate_response_legitimacy__adaptation_priority, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(clim_tr_t2020, climate_response_legitimacy__adaptation_priority, theater_ratio, 2020, 0.31).
narrative_ontology:measurement(clim_tr_t2030, climate_response_legitimacy__adaptation_priority, theater_ratio, 2030, 0.38).
narrative_ontology:measurement(clim_tr_t2040, climate_response_legitimacy__adaptation_priority, theater_ratio, 2040, 0.42).
narrative_ontology:measurement(clim_tr_t2050, climate_response_legitimacy__adaptation_priority, theater_ratio, 2050, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t2010, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(clim_be_t2020, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(clim_be_t2030, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2030, 0.61).
narrative_ontology:measurement(clim_be_t2040, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement(clim_be_t2050, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2050, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2010, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(clim_su_t2020, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement(clim_su_t2030, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2030, 0.67).
narrative_ontology:measurement(clim_su_t2040, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2040, 0.71).
narrative_ontology:measurement(clim_su_t2050, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2050, 0.73).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=2010, tn=2050
narrative_ontology:measurement(clim_grid_01, climate_response_legitimacy__adaptation_priority, accessibility_collapse(class), 2010, 0.48).
narrative_ontology:measurement(clim_grid_02, climate_response_legitimacy__adaptation_priority, accessibility_collapse(class), 2050, 0.74).
narrative_ontology:measurement(clim_grid_03, climate_response_legitimacy__adaptation_priority, accessibility_collapse(individual), 2010, 0.35).
narrative_ontology:measurement(clim_grid_04, climate_response_legitimacy__adaptation_priority, accessibility_collapse(individual), 2050, 0.68).
narrative_ontology:measurement(clim_grid_05, climate_response_legitimacy__adaptation_priority, accessibility_collapse(organizational), 2010, 0.42).
narrative_ontology:measurement(clim_grid_06, climate_response_legitimacy__adaptation_priority, accessibility_collapse(organizational), 2050, 0.71).
narrative_ontology:measurement(clim_grid_07, climate_response_legitimacy__adaptation_priority, accessibility_collapse(structural), 2010, 0.52).
narrative_ontology:measurement(clim_grid_08, climate_response_legitimacy__adaptation_priority, accessibility_collapse(structural), 2050, 0.76).
narrative_ontology:measurement(clim_grid_09, climate_response_legitimacy__adaptation_priority, resistance(class), 2010, 0.68).
narrative_ontology:measurement(clim_grid_10, climate_response_legitimacy__adaptation_priority, resistance(class), 2050, 0.75).
narrative_ontology:measurement(clim_grid_11, climate_response_legitimacy__adaptation_priority, resistance(individual), 2010, 0.58).
narrative_ontology:measurement(clim_grid_12, climate_response_legitimacy__adaptation_priority, resistance(individual), 2050, 0.71).
narrative_ontology:measurement(clim_grid_13, climate_response_legitimacy__adaptation_priority, resistance(organizational), 2010, 0.62).
narrative_ontology:measurement(clim_grid_14, climate_response_legitimacy__adaptation_priority, resistance(organizational), 2050, 0.73).
narrative_ontology:measurement(clim_grid_15, climate_response_legitimacy__adaptation_priority, resistance(structural), 2010, 0.74).
narrative_ontology:measurement(clim_grid_16, climate_response_legitimacy__adaptation_priority, resistance(structural), 2050, 0.76).
narrative_ontology:measurement(clim_grid_17, climate_response_legitimacy__adaptation_priority, stakes_inflation(class), 2010, 0.42).
narrative_ontology:measurement(clim_grid_18, climate_response_legitimacy__adaptation_priority, stakes_inflation(class), 2050, 0.81).
narrative_ontology:measurement(clim_grid_19, climate_response_legitimacy__adaptation_priority, stakes_inflation(individual), 2010, 0.28).
narrative_ontology:measurement(clim_grid_20, climate_response_legitimacy__adaptation_priority, stakes_inflation(individual), 2050, 0.82).
narrative_ontology:measurement(clim_grid_21, climate_response_legitimacy__adaptation_priority, stakes_inflation(organizational), 2010, 0.35).
narrative_ontology:measurement(clim_grid_22, climate_response_legitimacy__adaptation_priority, stakes_inflation(organizational), 2050, 0.79).
narrative_ontology:measurement(clim_grid_23, climate_response_legitimacy__adaptation_priority, stakes_inflation(structural), 2010, 0.38).
narrative_ontology:measurement(clim_grid_24, climate_response_legitimacy__adaptation_priority, stakes_inflation(structural), 2050, 0.77).
narrative_ontology:measurement(clim_grid_25, climate_response_legitimacy__adaptation_priority, suppression(class), 2010, 0.58).
narrative_ontology:measurement(clim_grid_26, climate_response_legitimacy__adaptation_priority, suppression(class), 2050, 0.71).
narrative_ontology:measurement(clim_grid_27, climate_response_legitimacy__adaptation_priority, suppression(individual), 2010, 0.32).
narrative_ontology:measurement(clim_grid_28, climate_response_legitimacy__adaptation_priority, suppression(individual), 2050, 0.74).
narrative_ontology:measurement(clim_grid_29, climate_response_legitimacy__adaptation_priority, suppression(organizational), 2010, 0.44).
narrative_ontology:measurement(clim_grid_30, climate_response_legitimacy__adaptation_priority, suppression(organizational), 2050, 0.72).
narrative_ontology:measurement(clim_grid_31, climate_response_legitimacy__adaptation_priority, suppression(structural), 2010, 0.52).
narrative_ontology:measurement(clim_grid_32, climate_response_legitimacy__adaptation_priority, suppression(structural), 2050, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.2).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_finance_adequacy).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, intergenerational_climate_justice).

% DUAL FORMULATION NOTE:
% This constraint is one reading (adaptation_priority) of the contested kernel climate_response_legitimacy. Sibling constraints with reading_ids mitigation_priority and degrowth_transformation instantiate competing readings of the same kernel. The three stories share a common referent (what counts as legitimate climate response) but author different ε values because the readings' structural distributions of benefits and harms differ. Adaptation-priority defers emissions reduction and extracts from low-income regions and future generations; mitigation-priority preserves growth-decoupling and extracts through technology-vendor lock-in and sovereign debt from carbon-pricing; degrowth-transformation requires wealthy-nation economic restructuring and extracts through work-time reduction and service provisioning. Each reading has its own beneficiary/victim set, enforcement mechanisms, and type classification. They do not resolve to a single answer but rather define a contestation space in climate-policy legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
