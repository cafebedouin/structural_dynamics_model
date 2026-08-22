% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Rapid Decarbonization Mandate (Mitigation Priority Reading)
 *   domain: climate_policy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the MITIGATION PRIORITY reading of the
 *   climate response obligation kernel. It models rapid decarbonization as
 *   the primary response to anthropogenic warming, grounded in
 *   intergenerational justice: future generations have a non-negotiable
 *   interest in inhabiting a climate system not destabilized by present-day
 *   emissions. The reading declares future generations and climate-vulnerable
 *   populations as primary beneficiaries (they face the lowest warming under
 *   this path) and incumbent fossil capital, carbon-intensive workers, and
 *   Global North heavy industry as victims (they bear transition costs and
 *   asset stranding). This constraint is one of three readings of the kernel;
 *   the adaptation_priority reading accepts higher warming and invests in
 *   resilience instead; the degrowth_reading rejects the efficiency-based
 *   transition and calls for reduced material throughput. This story models
 *   ONLY the mitigation-priority reading and treats it as a clean constraint
 *   with its own ε, beneficiary structure, and enforcement mechanism.
 *
 * KEY AGENTS:
 *   - future_generations: voiceless, powerless, civilizational time horizon — beneficiaries in the mitigation reading because minimizing warming reduces their adaptation burden
 *   - incumbent_fossil_capital: institutional power, biographical horizon — primary victim because assets become stranded under rapid phase-out policy
 *   - carbon_intensive_workers: moderate power, regional scope — victims bearing near-term income and retraining costs, constrained by place-based employment
 *   - developed_nation_governments: institutional power, agenda-setting role — enforce the mitigation mandate through carbon pricing, emissions targets, phase-out rules
 *   - global_south_governments: excluded from agenda-setting despite bearing both climate vulnerability and development constraints
 *   - climate_vulnerable_populations: powerless, global scope, highest physical risk — secondary beneficiaries in the mitigation reading
 *   - scientific_climate_community: analytical observer, credibility anchor for the intergenerational justice claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.52).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Rapid Decarbonization Mandate (Mitigation Priority Reading)").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '29e83463-11aa-4c1f-90b1-ec9933da0986').
narrative_ontology:cs_kernel_codification('29e83463-11aa-4c1f-90b1-ec9933da0986', distributed).
narrative_ontology:cs_authority_grounding('29e83463-11aa-4c1f-90b1-ec9933da0986', expertise).
narrative_ontology:cs_interpretation_layer_present('29e83463-11aa-4c1f-90b1-ec9933da0986').
narrative_ontology:cs_reading_relation('29e83463-11aa-4c1f-90b1-ec9933da0986', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('29e83463-11aa-4c1f-90b1-ec9933da0986', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('29e83463-11aa-4c1f-90b1-ec9933da0986', foundational, intergenerational_justice_primary).
narrative_ontology:cs_axiom_status(intergenerational_justice_primary, holdable).
narrative_ontology:cs_axiom_grounding('29e83463-11aa-4c1f-90b1-ec9933da0986', intergenerational_justice_primary, deontological).
narrative_ontology:cs_axiom('29e83463-11aa-4c1f-90b1-ec9933da0986', secondary, mitigation_feasibility_assumed).
narrative_ontology:cs_axiom_status(mitigation_feasibility_assumed, holdable).
narrative_ontology:cs_axiom_grounding('29e83463-11aa-4c1f-90b1-ec9933da0986', mitigation_feasibility_assumed, empirically_contingent).
narrative_ontology:cs_reference_frame('29e83463-11aa-4c1f-90b1-ec9933da0986', climate_stability_and_future_habitability).
narrative_ontology:cs_drift_state('29e83463-11aa-4c1f-90b1-ec9933da0986', contemporary_unmitigated_emissions_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('29e83463-11aa-4c1f-90b1-ec9933da0986', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, incumbent_fossil_capital).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, carbon_intensive_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_heavy_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, carbon_intensive_consumers_wealthy_nations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, carbon_intensive_consumers_wealthy_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot negotiate, exit, or reverse the climate system locked in by present emissions. They are the primary beneficiary group in the mitigation reading: every ton of CO2 avoided today reduces the warming they will inherit and the adaptation burden they will face. They have no voice in present governance.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Holds stranded assets (coal reserves, oil fields, gas infrastructure) rendered economically unviable by rapid decarbonization policy. They face balance-sheet write-downs, asset closures, and regulatory frameworks that restrict extraction. Exit to renewables is constrained by capital intensity and institutional lock-in.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, incumbent_fossil_capital, payer,
    institutional, biographical, constrained, global).

% Employed in coal, oil, gas, heavy manufacturing, and cement—sectors targeted for phase-out. They face near-term income loss and retraining burden with uncertain employment equivalence. Geographic and skills lock-in limit exit; transition assistance lags implementation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, carbon_intensive_workers, payer,
    moderate, biographical, constrained, regional).

% Steel, cement, chemicals, and petroleum refining in developed nations face sharpest mitigation burden through carbon pricing and emissions trading. Competitive disadvantage emerges where Global South producers face less stringent policy. Exit is constrained by technology costs and global supply chains.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_heavy_industry, payer,
    powerful, biographical, constrained, global).

% Small island nations, sub-Saharan Africa, South Asia, and deltaic regions face highest warming risks: displacement, food insecurity, resource scarcity. Mitigation minimizes these hazards. However, they contribute minimally to emissions and have minimal policy influence, creating asymmetry between risk exposure and decision-making power.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_populations, beneficiary,
    powerless, generational, trapped, global).

% Set and enforce mitigation policy through carbon pricing, emissions targets, renewable mandates, and subsidy removal. They articulate intergenerational justice framing and absorb political pressure from payers and negotiation with Global South. Their enforcement capacity determines whether mandate translates to real reductions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, developed_nation_governments, agenda_setter,
    institutional, generational, analytical, global).

% Solar, wind, battery, and grid technology producers benefit from subsidies, purchasing mandates, and guaranteed demand under mitigation policy. Strong incentives to lobby for rapid decarbonization. Profitability depends on sustained public support for the mitigation reading.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, global).

% Wealthy-nation consumers of high-carbon goods (aviation, meat, personal vehicles, heated housing) face higher costs through carbon pricing and consumption taxes. They benefit from reduced future warming but pay now. Political resistance from this seat drives policy softening.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, carbon_intensive_consumers_wealthy_nations, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, carbon_intensive_consumers_wealthy_nations, beneficiary).

% Caught between: their populations are climate-vulnerable but their development strategies depend on carbon-intensive industrialization. Mitigation reading marginalizes their voice by privileging intergenerational justice over North-South justice. They are excluded from agenda-setting despite bearing adaptation costs and losing development opportunity.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_governments, excluded,
    organized, generational, constrained, global).

% Supplies empirical foundation for mitigation reading through climate models, attribution science, and impact projections. Their credibility underpins the intergenerational justice claim. They face pressure to avoid politicization and account for deep uncertainty.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, scientific_climate_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, developed_nation_governments).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of atmospheric CO2 stabilization: individual rational behavior (producing and consuming carbon) creates a common-pool tragedy unless a coordinated constraint binds producers and consumers together to internalize the externality and distribute the burden of reducing atmospheric carbon concentration.
% TRANSFER_FUNCTION: Moves the cost of decarbonization from future generations (who would otherwise bear adaptation and mitigation costs under higher warming) to the present generation (who bear transition costs in labor, capital stranding, and consumption shifts). Also redistributes within the present generation: from fossil capital and carbon-intensive workers to renewable energy producers and climate beneficiaries.
% ABSENT_VOICES: Global South governments are excluded from agenda-setting despite bearing both vulnerability to climate harm and being constrained in development by Northern-set emissions limits. Low-income workers in transition economies have limited voice despite bearing concentrated income losses. Fossil fuel workers are heard in lobbying but not as primary participants in transition design. Unborn generations cannot speak for themselves and are represented only through proxy claims by present-day advocates.
% DISAPPEARANCE_RATIONALE: If the rapid decarbonization mandate and its enforcement mechanisms vanished overnight, global emissions would accelerate within years (removal of carbon pricing, renewable mandates, fossil fuel phase-out rules), greenhouse gas concentrations would continue rising unabated, and the climate trajectory that future generations inherit would shift toward higher warming, greater adaptation burden, and higher tipping-point risk. The present-day constraint is what makes lower warming paths possible; its absence guarantees higher warming.
% FOUNDING_PROBLEM: The atmosphere is a global commons; national emissions decisions generate planetary consequences that future generations cannot consent to or escape. Fossil fuel combustion externalizes climate harm: present producers and consumers do not bear the full cost of their carbon emissions, so market prices create insufficient incentive for decarbonization. Without coordinated policy to internalize the externality, rational individual and national behavior produces a climate trajectory incompatible with intergenerational justice.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by: (1) atmospheric science consensus that continued high emissions lock in future warming; (2) Global South governments and climate-vulnerable populations documenting present-day climate harms and future risks; (3) economic analyses showing market failure in carbon pricing; (4) independent moral philosophers and ethicists arguing intergenerational justice requires minimizing preventable harm to future generations. The fossil fuel industry disputes the founding problem or argues adaptation is cheaper than mitigation, but these are minority positions within the scientific and policy consensus.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because the constraint transfers substantial costs from future generations (who would otherwise bear adaptation under unmitigated warming) to present-day payers (fossil capital, workers, carbon-intensive producers, high-consumption wealthy-nation residents). Suppression is moderate (0.52): the constraint requires active enforcement through carbon pricing, regulatory phase-out, subsidy removal, and international agreements to overcome political resistance from payers and free-rider incentives. Theater ratio is moderate-high (0.41): the mitigation reading articulates a genuine coordination problem (tragedy of the commons in atmospheric carbon) and a real beneficiary structure (future generations), but enforcement capacity varies widely, governments struggle with credible commitment, and some climate policy is performative (counting toward targets without delivering emissions reductions). Accessibility collapse (0.62) is moderate: alternatives to the mitigation path exist (adaptation-first, degrowth, climate denial) and retain political currency, so the mitigation reading does not collapse them completely—though the climate science consensus and growing climate impacts do narrow the space for denialism. Resistance (0.71) is high: incumbent fossil capital actively lobbies against climate policy; carbon-intensive workers organize politically; Global South governments argue for differentiated responsibility; wealthy-nation consumers resist high-carbon taxes; oil-producing states oppose phase-out timelines. The constraint persists despite this high resistance because beneficiary pressure (scientific consensus, youth movements, vulnerable populations, political parties committed to climate action) is also strong.
 *
 * PERSPECTIVAL GAP:
 *   This reading privileges intergenerational justice (present generation's obligation to future generations) over intra-generational justice (differentiation between Global North and Global South, between wealthy and working-class present actors). The adaptive capacity and vulnerability profiles of agents differ starkly: wealthy nations can afford high transition costs and will adapt to higher warming better; developing nations face adaptation constraints and are locked into lower-cost carbon-intensive paths by global capital. The mitigation reading's beneficiary set (future generations) is intentionally voiceless and absent from negotiation, creating a principal-agent problem: present-day advocates speak for future generations, but those advocates may not represent the interests of future generations in the Global South or in the working class.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations have directionality d ≈ 1.0 (full targets in principle—they cannot exit, cannot refuse warming—but they are also beneficiaries in the mitigation reading—d ≈ 0.0 by the beneficiary axis). This paradox is resolved in the commentary: their role as beneficiaries dominates the metric. Fossil capital has directionality d ≈ 0.95 (target: loses assets, face phase-out policy, constrained exit). Carbon-intensive workers have directionality d ≈ 0.85 (target: lose income, constrained by place and skills, receive transition assistance that lags implementation). Climate-vulnerable populations have d ≈ 0.2 (slight beneficiary—they avoid worst warming under mitigation, but they also lack voice in the decision and may be exploited by transition mechanisms like carbon offsets). Developed-nation governments have d ≈ 0.5 (symmetric: they set the policy, but they also enforce it on their own populations and absorb political costs). No directionality overrides are needed; the derivation chain produces defensible values.
 *
 * MANDATROPHY ANALYSIS:
 *   The mitigation reading avoids simple mandatrophy (the founding problem is live—atmospheric carbon accumulation and climate risk continue), but faces two mandatrophy-adjacent risks: (1) THEATRICAL SUBSTITUTION: as actual emissions reductions slow (global emissions still rising despite Paris Agreement), policy enforcement increasingly focuses on counting, offsetting, and carbon markets rather than absolute emissions cuts; the theater_ratio rising from 0.28 to 0.41 over the interval captures this drift. (2) TRANSITION FATIGUE: the constraint's extraction (transition costs on workers and capital) persists decade after decade without clear endpoint—the high-extraction state becomes normalized and political pressure to ease the burden grows, creating risk that the mitigation commitment softens even as climate impacts accelerate. The founding_problem_status is LIVE because atmospheric carbon continues accumulating and future warming is still rising, but the founding_problem_corroboration is contested: fossil fuel interests and some economists argue adaptation is cheaper than mitigation and the founding problem is overblown. This contestation is structural, not an error in the reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_vs_intragenerational_justice,
    'Does the mitigation reading''s prioritization of intergenerational justice over intra-generational (North-South, class) justice accurately represent the beneficiary structure, or does it smuggle a hidden preference for present-day wealthy-nation interests?',
    'Comparative analysis of welfare gains to future generations under different burden-distribution schemes (equal per-capita reduction vs. historical responsibility vs. capacity-based); explicit valuation of adaptation costs to Global South present populations vs. avoided future warming for all; deliberative process including Global South voices in the set of parties whose interests define the constraint.',
    'If intra-generational justice is prioritized alongside or above intergenerational justice, the beneficiary set expands to include vulnerable present populations and the payer set contracts (fossil capital remains, but workers and Global South governments exit); the constraint type might shift from tangled_rope to piton (diffuse harms to many, concentrated benefit to Global North), or split into separate stories per the ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_vs_intragenerational_justice, preference, 'The hidden normative choice between intergenerational and intra-generational justice.').

omega_variable(
    discount_rate_and_future_valuation,
    'What discount rate applies to future harm from warming? Do we weight future generations'' interests equally with present interests, or do we discount future harm at a standard economic rate?',
    'Explicit ethical framework for intergenerational discounting adopted by the policy community; comparison of welfare calculations under different discount rates (0%, 1%, 3%, 7%) and their effect on the cost-benefit analysis of mitigation vs. adaptation.',
    'At standard economic discount rates (3-7%), adaptation becomes cost-effective relative to mitigation because future costs are heavily discounted; the mitigation constraint''s justification collapses and the reading becomes indefensible from an economic standpoint. At zero or near-zero discount rates, mitigation is strongly justified. This is a foundational assumption, not an empirical question, and different ethical frameworks produce different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discount_rate_and_future_valuation, preference, 'The normative choice of how to value future harm.').

omega_variable(
    mitigation_feasibility_and_tipping_points,
    'Is rapid decarbonization physically and technologically feasible within the timeline required to prevent dangerous warming? Do tipping points and thermal inertia in the climate system make present mitigation efforts insufficient to alter future warming trajectories?',
    'Empirical analysis of decarbonization rates achieved historically, projected renewable energy costs and deployment timelines, grid decarbonization and hard-sector (cement, steel, aviation) mitigation pathways, climate model ensemble runs with different mitigation scenarios and lag-response of the climate system to emissions reductions.',
    'If rapid decarbonization is infeasible or if climate system inertia means today''s mitigation efforts cannot prevent 2-3°C warming anyway, the mitigation reading''s beneficiary claim (future generations avoid worst warming) becomes false and the constraint becomes pure extraction disguised as coordination. The adaptation_priority reading gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_feasibility_and_tipping_points, empirical, 'Whether the mitigation path''s stated beneficiary outcome is achievable.').

omega_variable(
    measurement_and_enforcement_gap,
    'Can developed-nation governments credibly measure, verify, and enforce emissions reductions, or do carbon accounting, offsets, and loopholes hollow out the mitigation mandate?',
    'Comparison of self-reported vs. satellite-verified emissions; analysis of offset quality and permanence; tracking of whether Nationally Determined Contributions (NDCs) track to actual sector-level emissions reductions; litigation outcomes over alleged greenwashing.',
    'If the enforcement gap is large, the theater_ratio remains high and the constraint functions primarily as legitimacy performance rather than emissions reduction; the net effect on future generations'' welfare becomes ambiguous. Suppression may also be partially internalized: workers and capital accept the constraint believing it will deliver, but if enforcement is theatrical, their suffering is not compensated by the benefit they expect to generate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_and_enforcement_gap, empirical, 'The gap between mitigation policy as authored and mitigation as implemented.').

omega_variable(
    kernel_framing_alternative_axioms,
    'Is the climate_response_obligation kernel best framed as a matter of INTERGENERATIONAL JUSTICE (the mitigation reading''s core axiom) or as a matter of ECOLOGICAL LIMITS and PLANETARY BOUNDARIES (which would generate different readings emphasizing sufficiency over carbon-efficiency)?',
    'Philosophical analysis of competing frameworks; examination of how each axiom produces different responsibility assignments and different payer/beneficiary structures; deliberative process engaging ethical traditions that prioritize different values (justice, stewardship, sufficiency, resilience).',
    'If the ecological limits framing is adopted alongside intergenerational justice, the beneficiary set includes the biosphere and non-human life; the payer set expands from high-carbon humans to all humans above sustainable consumption levels; the constraint type might shift toward mandating aggregate human material reduction (degrowth reading) rather than technological decarbonization (mitigation reading). This is a conceptual choice that determines the entire structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_axioms, conceptual, 'Alternative framing of the kernel''s fundamental obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t5, climate_response_obligation__mitigation_priority, theater_ratio, 5, 0.32).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.36).
narrative_ontology:measurement(clim_tr_t15, climate_response_obligation__mitigation_priority, theater_ratio, 15, 0.39).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_response_obligation__mitigation_priority, theater_ratio, 25, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.41).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__mitigation_priority, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(clim_be_t5, climate_response_obligation__mitigation_priority, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(clim_be_t15, climate_response_obligation__mitigation_priority, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(clim_be_t25, climate_response_obligation__mitigation_priority, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__mitigation_priority, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t5, climate_response_obligation__mitigation_priority, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(clim_su_t15, climate_response_obligation__mitigation_priority, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(clim_su_t25, climate_response_obligation__mitigation_priority, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__mitigation_priority, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.25).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_fuel_phase_out_mandate).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, carbon_pricing_regime).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, just_transition_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_obligation kernel. Three structurally distinct constraints emerge from the kernel's three readings: mitigation_priority (this story), adaptation_priority (higher warming accepted; resilience invested), and degrowth_reading (material throughput reduction). Each has different ε (extracted from present generation for future benefit vs. adapted to higher baseline, vs. constrained through forced sufficiency), different beneficiaries (future generations vs. infrastructure-adapted regions, vs. global population at lower material throughput), and different payer sets. The readings coexist as live positions in political and scientific debate. This story models ONLY the mitigation_priority reading as a single constraint and links to the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__mitigation_priority, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
