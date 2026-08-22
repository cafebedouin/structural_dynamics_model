% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Material Throughput Reduction for Planetary Boundaries (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth reading of the climate
 *   response obligation kernel. The degrowth reading frames the binding
 *   constraint as material throughput reduction to stay within planetary
 *   boundaries, with sufficiency as the primary value and efficiency as
 *   secondary. This reading puts planetary systems and future humans as
 *   primary beneficiaries, names current Global North consumption classes and
 *   capital-accumulation-dependent sectors as victims, and treats capital
 *   accumulation itself as the extractive mechanism that must be constrained.
 *   The reading is live but contested — it coexists with mitigation-priority
 *   (decarbonization without throughput reduction) and adaptation-priority
 *   (accept warming, invest in resilience) framings. This story describes
 *   only the degrowth reading as a clean, ε-invariant constraint; the sibling
 *   readings are separate constraint stories with different ε values and
 *   different beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Planetary systems: primary beneficiary under this reading — reduced extraction pressure on biophysical cycles (carbon, nutrient, hydrological)
 *   - Global North consumption class: primary victim set — lifestyle and consumption reduction required to stay within boundaries
 *   - Global South development constrained: dual-positioned — constrained (payer) until North reduces, incidentally beneficiary (of reduced planetary pressure)
 *   - Capital accumulation dependent sectors: victim set — fossil fuels, extractive industries, growth-dependent finance — constrained by reduced throughput
 *   - Degrowth advocates/theorists: agenda-setters — author and promote the reading, set the interpretive frame
 *   - Future humans/descendants: beneficiary group — inherit preserved planetary capacity
 *   - Mitigation-priority proponents: excluded — would argue technology enables growth-with-decarbonization; not seated in this reading's authoring process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.78).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Material Throughput Reduction for Planetary Boundaries (Degrowth Reading)").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '06e6e124-a100-469b-b05d-754a9a60fa8a').
narrative_ontology:cs_kernel_codification('06e6e124-a100-469b-b05d-754a9a60fa8a', distributed).
narrative_ontology:cs_authority_grounding('06e6e124-a100-469b-b05d-754a9a60fa8a', distributed).
narrative_ontology:cs_reading_relation('06e6e124-a100-469b-b05d-754a9a60fa8a', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('06e6e124-a100-469b-b05d-754a9a60fa8a', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('06e6e124-a100-469b-b05d-754a9a60fa8a', foundational, material_throughput_reduction_necessary).
narrative_ontology:cs_axiom_status(material_throughput_reduction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('06e6e124-a100-469b-b05d-754a9a60fa8a', material_throughput_reduction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('06e6e124-a100-469b-b05d-754a9a60fa8a', foundational, sufficiency_before_growth).
narrative_ontology:cs_axiom_status(sufficiency_before_growth, holdable).
narrative_ontology:cs_axiom_grounding('06e6e124-a100-469b-b05d-754a9a60fa8a', sufficiency_before_growth, deontological).
narrative_ontology:cs_reference_frame('06e6e124-a100-469b-b05d-754a9a60fa8a', industrial_growth_metabolism).
narrative_ontology:cs_drift_state('06e6e124-a100-469b-b05d-754a9a60fa8a', contemporary_overshoot_recognition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('06e6e124-a100-469b-b05d-754a9a60fa8a', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_future_capacity).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumption_class).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, capital_accumulation_dependent_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_development_constrained).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_development_constrained).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, capital_accumulation_dependent_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The biophysical substrate: soil carbon stocks, oceanic heat capacity, atmospheric composition, freshwater aquifers, biodiversity. Named explicitly as primary beneficiary of throughput reduction. Reduced material extraction alleviates the overshoot pressure and permits ecosystem regeneration. This entity is non-agent but is the reading's primary claimant; it collects the constraint's primary benefit.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% High-income populations (North America, Western Europe, wealthy East Asia, Gulf states, affluent classes globally) whose per-capita consumption is 3–10x the sustainable global average. This reading requires downward lifestyle adjustment: energy consumption, material goods, meat consumption, international travel, housing and transportation per capita must contract. Exit from the constraint means either moving to lower-income regions (impractical at scale), defecting to alternative readings (mitigation-priority that permits growth via efficiency), or organizing political resistance. Their consumption patterns are named as victim under this reading.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumption_class, payer,
    organized, biographical, constrained, global).

% Lower-income and middle-income populations whose development pathways (rising material living standards, energy access, housing, infrastructure, education access) depend on resource extraction and manufacturing. Under this reading, their development is constrained: they cannot follow historical Northern industrialization pathways (which consumed massive resources per capita) and must wait for Northern reduction before their own growth can proceed. They are trapped between powerlessness to force Northern compliance and constraint from the reading itself. Incidentally, they benefit from reduced planetary pressure (less overshoot accelerates collapse risk to them especially), but the reading's mechanism falls on them as a side effect.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_development_constrained, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_development_constrained, beneficiary).

% Fossil fuel extraction, mining, large-scale manufacturing, consumer goods retail, real-estate development, financial services dependent on growth-based returns (equity funds, bonds, pension funds). Under this reading, capital accumulation itself becomes the extractive mechanism — profit, rents, and growth-dependent returns are structurally incompatible with throughput reduction. These sectors bear costs directly: shrinking throughput means shrinking material flows, shrinking revenue, shrinking profit opportunities. Exit is available only by relocating to nations that adopt alternative readings (mitigation-priority, adaptation-priority) and maintaining growth, which preserves the global constraint. They are named as victim group and structural adversaries of the reading.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, capital_accumulation_dependent_sectors, payer,
    organized, biographical, constrained, global).

% Humans not yet born (and current generations too young to participate in current decision-making). They benefit from reduced planetary extraction pressure in the form of preserved biophysical capacity, avoided tipping points, inherited ecosystem services, and stable climate within a narrower warming range. They cannot exit or defend themselves — their interests are represented via proxy by advocates and activists. This reading vindicates their moral claim against present-generation consumption.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_humans_and_descendants, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, future_humans_and_descendants).

% Academics, policy advocates, civil society actors (unions, environmental groups, climate-justice movements), and activists who author and promote the degrowth reading. They set the interpretive frame: name the primary beneficiary (planetary systems, future humans), define the victim group (Northern consumption, capital sectors), articulate the founding problem (boundaries are binding, decoupling has failed), and push for adoption through research, advocacy, mobilization, and political messaging. Their power is growing (especially in younger cohorts and climate-vulnerable regions) but remains constrained by institutional resistance, capital's political influence, and the dominant mitigation-priority framing in IPCC/UNFCCC and most governments.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_advocates_and_theorists, agenda_setter,
    moderate, generational, mobile, global).

% IPCC, UNFCCC, regional climate bodies, scientific consensus structures. They produce empirical evidence supporting the constraint's founding problem: carbon budgets are finite, planetary boundaries are binding, current trajectories lead to high-impact warming and ecosystem collapse. However, they frame the response institutionally through mitigation-priority (rapid decarbonization, technology, efficiency) rather than degrowth (sufficiency, throughput reduction). This reading is not their dominant institutional interpretation, though degrowth advocates cite their empirical findings to support it.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, international_climate_bodies, observer,
    institutional, generational, constrained, global).

% Governments, corporations, green-tech industries, climate policy makers, many economists committed to rapid decarbonization while maintaining or modestly growing material throughput. They would strongly object to this reading if present: they argue technology and efficiency gains (renewable energy, circular economy, electric vehicles, sustainable materials) can decouple growth from carbon, that consuming societies need not downscale lifestyles, and that degrowth is politically impossible and normatively objectionable (denies development rights to lower-income people, reduces welfare). Their exclusion from this reading's authoring seat is structural — their presence would reframe the constraint as a false dichotomy (decarbonization vs. degrowth, when efficiency could solve it). They are not seated in this constraint story's process.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mitigation_priority_proponents, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of allocating finite planetary extraction capacity across current and future human populations and non-human systems. Under competitive market logic (current state), each actor maximizes individual consumption, leading to collective overshoot and ecosystem collapse. The reading proposes sufficiency-based coordination: material throughput contracts to a per-capita level sustainable at global adoption, and allocation shifts from market competition to needs-based distribution plus equity allocations for development catch-up. This requires genuine coordination — binding allocations, transparency, verification, and enforcement across all major extraction regions. Unilateral Northern reduction without global coordination is costly for Northern actors (economic disadvantage if other regions continue growth) and ineffective (global boundaries require global participation). The coordination function is real.
% TRANSFER_FUNCTION: Moves material throughput reduction obligations onto high-consuming populations (Global North classes, capital sectors) and constrains development pathways for lower-income populations (Global South, unless North reduces first). The transfer is not a monetary flux but a capacity constraint: access to extracted material, energy services, development growth, and capital accumulation opportunities moves from current-generation high-consumption actors toward (1) planetary regeneration (reduced biophysical extraction pressure), (2) future-generation capacity (preserved ecosystem services, climate stability), and (3) lower-income populations (permitted development after Northern sufficiency). Alternatively stated: the reading reallocates the 'safe operating space' of planetary boundaries away from Northern growth and capital accumulation and toward planetary regeneration and equitable development.
% ABSENT_VOICES: Mitigation-priority proponents and adaptation-priority advocates are excluded from this reading's authoring seat. They would argue: (1) technology and efficiency enable growth-with-decarbonization, (2) sufficiency is politically impossible in democratic societies, (3) adaptation is more cost-effective than degrowth, and (4) denying development to Global South is unjust. Fossil fuel industries and growth-dependent capital would object loudly if present, arguing the reading is anti-growth bias and politically unviable. Their absence from the authoring process means the reading's definition of the problem and solution is not contested by the most powerful institutional actors who would lose from it.
% DISAPPEARANCE_RATIONALE: If this reading and its enforcement mechanisms (throughput reduction obligations, sufficiency-based allocation, capital constraints) disappeared overnight, material extraction and consumption would revert to competitive depletion trajectories under either mitigation-priority framing (decarbonization without throughput reduction, betting on decoupling) or adaptation-priority framing (accept warming, invest in resilience). Biophysical systems would continue overshoot trajectories; planetary boundaries would continue to transgress; Global South development would accelerate under Northern growth continuation. The world arrangement is stable under the alternative readings' framings but structurally unstable under degrowth's real-world constraints (boundaries are physics, not policy, and current trajectories overshoot them). This reading's disappearance would be registered as a structural political-economic shift, not as a natural settlement — it would represent institutional victory for the competing readings and defeat for the sufficiency framing.
% FOUNDING_PROBLEM: Biophysical planetary boundaries (carbon-cycle capacity, nutrient cycling, freshwater depletion, land-use saturation, biodiversity loss, ocean acidification) are approaching and in some cases transgressing hard ecological limits. Current material throughput (~100+ billion tonnes annually of biomass, fossil fuels, metals, minerals extracted globally) is unsustainable at global adoption and would require 1.6–2.0 Earths to regenerate. Efficiency improvements (relative decoupling: lower carbon per unit GDP) have failed to achieve absolute decoupling (reduction in total resource extraction and carbon emissions) at the scale and pace needed, and are typically overwhelmed by rebound effects (lower costs increase consumption). The core founding problem is that planetary boundaries are non-negotiable physical facts, not policy choices, and current trajectories lead to ecosystem collapse, cascade failures, and civilizational constraints within decades unless material throughput contracts sharply.
% FOUNDING_PROBLEM_CORROBORATION: Planetary Boundaries Framework (Rockström et al., Nature 2009; updated 2023) documents transgression of multiple boundaries (climate, biodiversity, land use, nutrient cycles, ocean acidification) with high confidence. IPCC Special Report on 1.5°C (2018) establishes carbon-budget constraints (580 Gt CO2 remaining for 50% chance of 1.5°C); subsequent tracking shows budgets tightening, not expanding. Global Resource Outlook (IRP, UNEP) documents growing material extraction (160+ billion tonnes by 2050 baseline) and failed decoupling. Material Flow Analysis across high-income economies shows efficiency gains consistently overwhelmed by consumption growth (rebound effect). Post-Kyoto empirical analysis shows relative decoupling without absolute decoupling (carbon per GDP down, total carbon up). The founding problem is corroborated by Earth-system science, resource economics, and ecological analysis from sources outside the degrowth-reading benefiting parties. Mitigation-priority and adaptation-priority proponents concede the founding problem is live (boundaries are real, current trajectories are unsustainable) but dispute the necessity and desirability of the proposed remedy. The corroboration is solid from empiricists and Earth scientists; the dispute is over response, not over the diagnosis.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint imposes asymmetric costs: Northern lifestyles must contract significantly, capital accumulation must slow or reverse, development pathways for lower-income populations are constrained unless Northern reduction happens first. This is not a Pareto-improvement; it creates clear losers (Northern middle and upper classes, capital sectors). Suppression is high (0.72) because enforcing throughput reduction requires active mechanisms: carbon pricing with sufficiency thresholds, production/consumption regulations, international allocation agreements, and countering the political power of capital-dependent sectors. Theater is moderate (0.41) because much of the apparent enforcement activity is genuinely coordinating (tracking, verification, allocation) but some is performative — pledges to reduce that avoid binding targets, corporate 'net zero' claims that rely on unverified offsets, and efficiency improvements that don't reduce absolute throughput. The measurement series show extractiveness and suppression rising through t=20 then plateauing at t≥25, modeling a scenario where enforcement intensity stabilizes as the reading's institutionalization reaches saturation (hard ceiling on what enforcement machinery can achieve without political collapse). Theater ratio rises slightly then stabilizes as the separation between ambitious targets and actual reduction becomes harder to hide as time passes.
 *
 * PERSPECTIVAL GAP:
 *   The most stark divergence is between the global_north_consumption_class seat and the planetary_systems seat. From the Northern consumption perspective, this reading is extractive — it imposes forced reduction, constrains choice, and demands sacrifice with uncertain payoff (other regions may not comply, so the sacrifice may be wasted). From the planetary systems perspective (speaking through Earth-system science), the constraint is pure necessity — the overshoot is active extraction of planetary capacity and the reading simply names it. The International climate bodies (observer seat) sit ambiguously: they produce data supporting the founding problem (boundaries are real, throughput is unsustainable) but institutionally promote mitigation_priority framing (technology can solve this without lifestyle change), which implicitly rejects this reading's classification of capital accumulation as extractive. The mitigation-priority agenda-setters (excluded) would compute this reading as deeply extractive to themselves (threatening their model and their profit opportunities) while calling degrowth advocates' computation a 'catastrophism bias.' The engine computes these divergences from the structural data — the authored claim and metrics are independent, and the divergence is itself the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply across stakeholder seats. Planetary systems and future humans (beneficiary roles) have d near 0.0 (full beneficiaries — they collect reduced extraction pressure with zero cost to themselves, though human descendants inherit reduced material availability). The Global North consumption class (payer role, organized power) has d near 0.85 (full target of extraction — they bear the cost of reduction, have constrained exits, face lifestyle downward adjustment). Capital sectors (payer role, organized/institutional power) have d near 0.90 (full targets — profit opportunities shrink with throughput reduction; exit is only available to those who can relocate to adapter-priority or mitigation-priority jurisdictions, which preserves the constraint globally). The Global South development group (dual role — constrained payer, incidental beneficiary) has d near 0.70 (high target, but asymmetrically — they are constrained by Northern behavior, not by direct extraction from them; they would benefit if Northern reduction happened, but waiting for it is its own cost). Degrowth advocates (agenda-setter role, moderate power) have d near 0.35 (moderately beneficiary — the reading empowers them, advances their reading, and vindicates their decades of advocacy; they collect status, intellectual authority, and policy influence). This directionality structure is the core of the tangled_rope classification: genuine coordination function (solving planetary boundaries, allocating finite capacity globally) paired with asymmetric extraction (costs concentrated on high-consuming actors, benefits concentrated on planetary systems and future humans).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading faces a mandatrophy risk: the founding problem (biophysical limits are binding) is live and empirically clear, but the constraint's response (material throughput reduction, sufficiency-based coordination) is institutionally rejected across most of the world. Governments and corporations predominantly adopt the mitigation_priority reading (decarbonization without throughput reduction), treating degrowth as politically impossible. If this reading's mandate (planetary boundaries require sufficiency) is correct but the enforcement mechanism (throughput reduction) remains incompatible with capital accumulation logic, then the constraint faces institutional death even if the founding problem persists. This is not true mandatrophy (founding problem dead, constraint persists) — it is mandate rejection: the founding problem remains alive, but the response is structurally suppressed by the organized power of capital sectors and the political viability of efficiency-focused alternatives. The reading avoids mandatrophy by remaining a live proposal (degrowth advocacy is growing, especially among younger cohorts and in climate-vulnerable regions), but it operates under severe institutional headwinds. The theater ratio stabilizing at 0.41 reflects this: some genuine coordination activity (scientific collaboration on boundaries, voluntary sufficiency movements) but substantial performative activity (efficiency pledges that avoid throughput reduction, corporate greenwashing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rebound_effect_magnitude,
    'Does efficiency-driven decoupling (the mitigation-priority mechanism) actually achieve absolute resource reduction at a sufficient pace and scale, or do rebound effects systematically overwhelm efficiency gains?',
    'Long-term data on absolute resource extraction, energy use, and carbon emissions in high-income economies after 20+ years of efficiency improvements (LED lighting, efficient appliances, renewable energy deployment). Track whether efficiency gains compress material consumption or expand consumption elsewhere (air travel, data centers, new consumer categories).',
    'If rebound effects are dominant (high-confidence finding), the mitigation-priority reading is structurally dependent on hopes that future technology will break the pattern — degrowth''s claim that sufficiency is necessary becomes more plausible. If genuine decoupling is achieved, the mitigation-priority reading''s viability strengthens and degrowth becomes less structurally justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rebound_effect_magnitude, empirical, 'Whether decoupling through efficiency is sufficient or whether throughput reduction is necessary.').

omega_variable(
    global_south_development_constraint_justice,
    'Is it just to constrain Global South development pathways (even temporarily, ''until North reduces first'') when those regions bear lower responsibility for historical overshoot and face immediate development needs?',
    'Philosophical and political analysis of distributive justice, historical responsibility, and capability — this is not empirically resolvable but depends on which normative framework governs intergenerational and inter-regional equity. Degrowth advocates argue historical justice requires North-first reduction; growth-prioritizers argue denying development violates present-generation rights.',
    'If the justice question is resolved against degrowth (present-generation development rights take priority), the constraint''s victim classification changes: Global South becomes beneficiary (development permitted under Northern compliance with carbon budgets) rather than victim (constrained). This would reclassify the reading''s core distributional claim and weaken the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_south_development_constraint_justice, preference, 'Whether constraining Global South development pathways is a legitimate cost of the degrowth reading.').

omega_variable(
    institutional_feasibility_of_enforcement,
    'Is democratic enforcement of throughput reduction feasible, or does the constraint require authoritarian mechanisms incompatible with the values (equity, justice, self-determination) that justify the reading?',
    'Historical and forward-looking analysis of how binding allocation constraints are enforced across populations. If degrowth is attempted as policy, what enforcement mechanisms emerge — carbon ration cards, consumption restrictions, border controls? Do they maintain democratic legitimacy, or do they collapse it?',
    'If degrowth enforcement requires authoritarian suppression of Northern consumption classes, the reading''s normative justification (justice, planetary survival) enters contradiction with its mechanism (coercive constraint). This would create a second-order mandatrophy: the constraint is needed but cannot be legitimately enforced. If democratic enforcement emerges and holds, the constraint''s institutional viability increases sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_feasibility_of_enforcement, empirical, 'Whether degrowth enforcement is institutionally achievable under democratic constraints.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Do the mitigation-priority and adaptation-priority readings logically foreclose degrowth''s core premise (that sufficiency is necessary) or do they coexist as live alternatives that different parties defend?',
    'Examine whether the three readings'' core premises can be held simultaneously within a single framework or whether they logically contradict. Mitigation-priority says ''decoupling is achievable''; degrowth says ''decoupling has failed and will not scale sufficiently.'' Adaptation-priority says ''acceptance of warming is cost-effective''; degrowth says ''warming is intolerable and requires prevention-first.'' Can a party hold all three positions?',
    'If mitigation-priority and degrowth are logically contradictory at their foundations, then one reading forecloses the other, and the constraint relationship is competition-to-resolution (not coexistence). If they coexist as different empirical bets (does decoupling work?) or different value orderings (is development or planetary preservation primary?), then the readings are sisters, not adversaries. The kernel''s structure determines whether this becomes a foreclosure-driven conflict or a coexistence-accommodating debate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Whether degrowth, mitigation-priority, and adaptation-priority readings foreclose one another or coexist as live alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t5, climate_response_obligation__degrowth_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__degrowth_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(clim_tr_t15, climate_response_obligation__degrowth_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__degrowth_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(clim_tr_t25, climate_response_obligation__degrowth_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__degrowth_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(clim_tr_t35, climate_response_obligation__degrowth_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(clim_be_t5, climate_response_obligation__degrowth_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__degrowth_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(clim_be_t15, climate_response_obligation__degrowth_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__degrowth_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(clim_be_t25, climate_response_obligation__degrowth_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__degrowth_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(clim_be_t35, climate_response_obligation__degrowth_reading, base_extractiveness, 35, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(clim_su_t5, climate_response_obligation__degrowth_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__degrowth_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(clim_su_t15, climate_response_obligation__degrowth_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__degrowth_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(clim_su_t25, climate_response_obligation__degrowth_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__degrowth_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(clim_su_t35, climate_response_obligation__degrowth_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate response obligation kernel. The kernel is the binding commitment to respond to climate change and planetary overshoot. Three structurally distinct readings instantiate different constraints from this shared kernel: (1) DEGROWTH READING (this story, constraint_id: climate_response_obligation__degrowth_reading): material throughput reduction, sufficiency primary, planetary systems as primary beneficiary, capital accumulation as extractive mechanism — high extractiveness (0.78), tangled_rope. (2) MITIGATION-PRIORITY READING (sibling, constraint_id: climate_response_obligation__mitigation_priority): rapid decarbonization with modest growth, efficiency primary, intergenerational human welfare as primary beneficiary, assumes technological decoupling — moderate extractiveness, rope or tangled_rope. (3) ADAPTATION-PRIORITY READING (sibling, constraint_id: climate_response_obligation__adaptation_priority): accept 2–3°C warming, invest in resilience, human adaptation capacity as primary beneficiary, minimize abatement costs — low-to-moderate extractiveness, rope. Each reading has different ε-invariance (different referents lead to different extraction values), different beneficiary/victim structures, and different classifications. They coexist in political discourse across different institutional and geographic strongholds. The three stories are linked by network.affects_constraints for contamination propagation analysis: if one reading's institutional viability degrades, the others are affected (e.g., if decarbonization-only proves insufficient, pressure shifts toward degrowth; if warming accelerates, adaptation moves from niche to mainstream).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, analytical, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
