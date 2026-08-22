% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Climate Response Obligation (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading of climate response obligation frames planetary
 *   biophysical limits as the primary constraint on human activity. Material
 *   throughput — extraction, processing, transport, disposal — must be
 *   reduced to stay within regenerative capacity, especially in the Global
 *   North. This reading subordinates growth-maximization and development
 *   aspirations (both North and South) to biophysical necessity. Sufficiency
 *   — meeting needs adequately rather than maximizing consumption — becomes
 *   the organizing principle. The constraint is CLAIMED as tangled_rope
 *   (coordination on throughput + asymmetric cost to consumption class) while
 *   metrics describe substantially extractive, actively enforced operation
 *   with rising theater: some policy measures are genuine transition
 *   infrastructure, others perform reduction without delivering it. The
 *   measurement series span an observed interval (0–16) and projected
 *   interval (24–40) aligned to a unified time grid.
 *
 * KEY AGENTS:
 *   - Planetary biosphere: structural beneficiary, no voice, target of extraction pressure reduction
 *   - Future generations: structural beneficiary, powerless, no negotiating seat
 *   - Global North consumption class: target/payer (powerful but constrained), faces lifestyle reduction and identity-fusion costs
 *   - High-carbon capital owners: institutional payer, faces growth constraint and forced business-model reorientation
 *   - Global South subsistence populations: excluded agent (should benefit from development rights), voice suppressed by North-first principle
 *   - Regenerative transition architects: organized beneficiary, mobile, gain institutional demand from degrowth framework
 *   - Policy implementers: institutional agenda-setter, constrained exit, manage irreconcilable political pressures
 *   - Mitigation-priority and adaptation-priority advocates: organized excluded voices, would reshape constraint if admitted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.78).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.71).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Climate Response Obligation (Degrowth Reading)").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, 'e5aa909b-a0db-4831-bacf-340564e5b6e7').
narrative_ontology:cs_kernel_codification('e5aa909b-a0db-4831-bacf-340564e5b6e7', distributed).
narrative_ontology:cs_authority_grounding('e5aa909b-a0db-4831-bacf-340564e5b6e7', distributed).
narrative_ontology:cs_reading_relation('e5aa909b-a0db-4831-bacf-340564e5b6e7', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('e5aa909b-a0db-4831-bacf-340564e5b6e7', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('e5aa909b-a0db-4831-bacf-340564e5b6e7', foundational, material_reduction_necessary).
narrative_ontology:cs_axiom_status(material_reduction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e5aa909b-a0db-4831-bacf-340564e5b6e7', material_reduction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('e5aa909b-a0db-4831-bacf-340564e5b6e7', foundational, sufficiency_principle_primacy).
narrative_ontology:cs_axiom_status(sufficiency_principle_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e5aa909b-a0db-4831-bacf-340564e5b6e7', sufficiency_principle_primacy, deontological).
narrative_ontology:cs_axiom('e5aa909b-a0db-4831-bacf-340564e5b6e7', secondary, north_first_allocation).
narrative_ontology:cs_axiom_status(north_first_allocation, holdable).
narrative_ontology:cs_axiom_grounding('e5aa909b-a0db-4831-bacf-340564e5b6e7', north_first_allocation, deontological).
narrative_ontology:cs_reference_frame('e5aa909b-a0db-4831-bacf-340564e5b6e7', steady_state_regenerative_equilibrium).
narrative_ontology:cs_drift_state('e5aa909b-a0db-4831-bacf-340564e5b6e7', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5aa909b-a0db-4831-bacf-340564e5b6e7', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_biosphere).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_development_rights).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumption_class).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, high_carbon_capital_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, regenerative_transition_architects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The biophysical system subject to extraction via material throughput. Under degrowth reading, the constraint aims to reduce pressure on biogeochemical cycles, regenerative capacity, and climate stability. This is an abstract entity, not a negotiating party, but the reading's core beneficiary claim centers it as the primary receiver of benefit.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_biosphere, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_biosphere).

% Inherit the consequences of current material throughput choices. They have no voice in present decisions but bear the extraction pressure of high-throughput equilibrium. The degrowth reading constitutes them as primary beneficiaries of reduced material demand, which preserves livable conditions. They cannot negotiate or exit.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, future_generations).

% Maintains high per-capita material consumption (energy, goods, food) at 5–10x global average. Under degrowth reading, this class must reduce absolute material demand, not just carbon intensity. Exit via offshoring (carbon leakage, outsourced extraction) is gradually closed by scope of the constraint. Lifestyle reduction is the material cost; cultural identity and status systems built on consumption are the identity-fusion cost.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumption_class, payer,
    powerful, biographical, constrained, global).

% Accumulate value by extracting and processing material throughput: fossil fuel producers, automotive/aviation manufacturers, industrial agriculture operators, mining conglomerates. Degrowth reading frames capital accumulation itself as the mechanism sustaining extraction pressure. They bear the cost of constrained growth and material-intensity reduction. Their exit options are reorientation to circular/regenerative models (costly, structurally opposed by incumbent value chains) or relocation to jurisdictions without degrowth enforcement (regulatory arbitrage, progressively closed as adoption spreads).
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, high_carbon_capital_owners, payer,
    institutional, biographical, constrained, global).

% Depend on access to material resources (land, water, minerals, energy) for basic needs. Degrowth reading subordinates their development to North reduction: material budget is allocated to survival in the South only AFTER North reduces. They are excluded from the negotiation; their situation is determined by the outcome. Voices from development-focused economists, post-colonial scholars, and Global South governments attest that this reading subordinates justice to environmental constraint without offering development pathways.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_subsistence_populations, excluded,
    powerless, biographical, trapped, global).

% Design and implement circular economy, regenerative agriculture, renewable energy systems, and degrowth transition infrastructure. They benefit from adoption of the degrowth framework: policy support, capital reallocation, and mandate for systems redesign create institutional demand for their expertise and models. Mobility comes from their technical/organizational skills; they can shift between jurisdictions and sectors.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, regenerative_transition_architects, beneficiary,
    organized, generational, mobile, global).

% The physical system stabilized by reduced material throughput. Under degrowth reading, the constraint aims at maintaining the climate system within a habitable envelope by reducing cumulative greenhouse gas emissions and land-use pressure. Like the biosphere, this is an analytical entity representing the physical referent of the constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, climate_system, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, climate_system).

% Governments and regulatory bodies that enforce degrowth constraints: material quotas, carbon budgets, consumption standards, production limits, land-use restrictions. They set the rules, administer allocation, and defend the system against exit/circumvention. They face political pressure from both the Global North consumption class (opposing constraint) and Global South populations (opposing subordination). Their enforcer role is power; their constrained exit comes from the difficulty of maintaining legitimacy across irreconcilable demands.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, policy_implementers, agenda_setter,
    institutional, generational, constrained, national).

% Argue that rapid decarbonization without material degrowth is possible via efficiency, electrification, and carbon capture; that imposing degrowth on current generations violates intergenerational fairness and Global South development rights. They are excluded from this constraint's beneficiary/payer structure by the degrowth reading's core premise rejection of efficiency-without-reduction. Their position would reshape the constraint if admitted.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mitigation_priority_advocates, excluded,
    organized, generational, mobile, global).

% Argue that 2–3°C warming is inevitable and unavoidable; that resources should prioritize resilience and adaptation rather than costly prevention/reduction. They are excluded from this reading's coordination function. Their adoption would shift the constraint's purpose from throughput reduction to adaptation infrastructure. Legislative testimony and policy forums document their active exclusion from degrowth agendas.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, adaptation_priority_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a transition to material-sufficiency equilibrium: establishes shared limits on annual material throughput, allocates remaining budget toward survival and essential flourishing (prioritizing Global South), and reorganizes economic activity around regeneration rather than accumulation. The problem solved is the biophysical impossibility of infinite-growth extraction on a finite planet — participants must coordinate on lower throughput or risk collapse.
% TRANSFER_FUNCTION: Moves access to material resources and atmospheric carbon space away from high-consumption Global North populations and capital-intensive industries toward (1) preserved biosphere capacity, (2) Global South development rights, (3) regenerative economic models. In the short term, it transfers material hardship: Global North consumption-class populations experience reduced goods, energy, mobility; capital owners face constrained growth and forced business-model transitions.
% ABSENT_VOICES: Global South subsistence populations and post-colonial nations are structurally excluded: they have no seat in setting the material budget or allocation principles, though the constraint determines whether they can develop. Mitigation-priority advocates and adaptation-priority advocates are excluded: they argue the premise (whether degrowth is necessary) should be contested, but the degrowth reading forecloses that debate within its own frame. Indigenous land-based economies (which often operate at low throughput) are absent from the design conversation despite their relevance as tested models.
% DISAPPEARANCE_RATIONALE: If this constraint (material-throughput limits, North-first reduction, sufficiency-over-efficiency framing) disappeared, consumption-class expansion would resume, capital accumulation would accelerate material extraction, and Global South development would compete for throughput alongside North consumption at current rates. The biophysical pressure would intensify, ecological tipping points would move closer, and claims on planetary capacity would skyrocket. The material economy would reorganize toward high-throughput equilibrium; the constraint's absence would be constitutive for that reorganization.
% FOUNDING_PROBLEM: Planetary boundaries are being exceeded: material throughput (land use, mineral extraction, water depletion, nutrient cycling) is unsustainable at current levels in the Global North and becomes unsustainable globally if universalized. Efficiency gains have been overwhelmed by consumption growth (Jevons paradox). Only absolute reduction in material throughput, concentrated in the highest-consuming populations, can stay within biophysical limits while preserving development space for the Global South.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (biophysical limits exceeded at current North consumption) is documented by Planetary Boundaries framework (Steffen et al.) and endorsed by earth-systems scientists, ecological economists, and Global South governments. The solution (North-first degrowth as necessary and sufficient) is contested: mitigation-priority researchers argue efficiency+electrification can decouple growth from impact; adaptation-priority voices argue the warming is already locked in and adaptation is the only viable response; mainstream climate policy (IPCC synthesis) assigns degrowth a minor role and emphasizes mitigation via decarbonization. No broad corroboration exists from outside degrowth-committed movements.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness at 0.78 is high because the constraint transfers material livelihood from a dominant consumption class to a non-negotiating planetary system. Suppression at 0.71 is also high: the constraint persists by actively excluding alternative readings (mitigation, adaptation) and silencing Global South objections to North-first allocation. Theater at 0.42 reflects that degrowth policy includes genuine transition infrastructure (renewable energy, regenerative agriculture research, circular economy design) alongside performative measures (carbon offsets that don't reduce throughput, 'sustainable growth' rhetoric that embeds efficiency gains while throughput rises). The measurement series show extractiveness and suppression rising from observed baseline (0.62, 0.58) toward plateau (0.78, 0.71) as enforcement machinery hardens and exit routes close: this is extraction accumulation over the interval, with theater ratio rising as policy intensity increases but structural change lags. Accessibility collapse shows different trajectories at different levels: individual-level alternatives (migration, consumption switching) collapse fastest (0.45→0.72); organizational alternatives (business-model shifts, capital reallocation) collapse more slowly (0.58→0.78); class-level alternatives (working-class coalition against reduction) persist (0.62→0.68) because resistance remains high; structural-level alternatives (techno-optimism, adaptation-focus) plateau (0.68→0.72) as institutional commitment solidifies. Stakes inflation follows a similar pattern: highest at organizational level (capital faces forced transition, 0.61→0.81) and structural level (entire growth model under pressure, 0.71→0.78), lower at class level where solidarity and mutual aid partially buffer individual costs (0.48→0.62). Resistance is highest at the structural level (0.81 initial, 0.75 final) and organizational level (0.78→0.74) where stakes are steepest, declining over time as either enforcement succeeds or organized opposition achieves legitimacy and policy influence. The grid documents a system hardening enforcement and collapsing alternatives while resistance persists and even intensifies at certain levels — a tightening tangled rope where coordination is declared but extraction is what's being enforced.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (policy implementers) and the beneficiary-declaring seat (regenerative architects + climate advocates) perceive this constraint as genuine coordination: a necessary transition to a livable planetary equilibrium. The Global North consumption class and capital-owner seats perceive it as enforced extraction: a transfer of material livelihood and economic growth opportunity to an abstract planetary system that cannot negotiate, compensate, or be held accountable. The Global South excluded seat perceives it as a subordination of development rights: material scarcity is being imposed by the North's prior accumulation, now dressed up as planetary justice. The engine should compute these divergences from the power/exit/role data: high-power payer seats with constrained exit should register low perception of coordination; powerless excluded seats should register the deepest asymmetry. The authorized mitigation and adaptation voices, if they were seats rather than excluded agents, would perceive the constraint as a false choice: efficiency + electrification (mitigation reading) or adaptation infrastructure (adaptation reading) would solve the founding problem without imposing material reduction on current generations.
 *
 * DIRECTIONALITY LOGIC:
 *   The biosphere and future generations are structural beneficiaries (d → 0.0): the constraint aims to reduce extraction pressure on them. They have no negotiating power and no exit — they are purely targets of the constraint's protective aim. Regenerative transition architects are beneficiaries (d moderate, ~0.3): they gain institutional demand and policy support but are not the primary receivers of benefit; they are coordinators/enablers. The Global North consumption class is a high-target payer (d → 1.0): they bear direct material costs (reduced consumption, lifestyle change), have constrained exit (carbon leakage is being closed), and are powerless to block the constraint through legitimate channels — their power exists, but it operates in the market for goods and capital, not in the institutional setting where degrowth is established. High-carbon capital owners are institutional targets (d → 0.85): they face forced business-model reorientation, growth constraints, and reputational/regulatory pressure. Their institutional power is substantial, but it's being overwhelmed by coordinated policy + moral authority of the climate crisis. Global South subsistence populations are conceptually beneficiaries (lower material pressure, more development space) but operationally victims (excluded from allocation decisions, development delayed until North reduces) — their directionality is ambiguous and contested; the reading subordinates them to Northern reduction, which should compute as a high extraction cost (d → 0.75) despite the nominal benefit language. Policy implementers sit at d ~ 0.5 (symmetric): they enforce the constraint and face political pressure from all sides; they gain legitimacy from crisis response but lose authority as resistance grows.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (planetary boundaries exceeded at current throughput) is live and increasingly recognized by earth-system science and policy forums. However, the problem-solution fit is contested: the degrowth reading CLAIMS material reduction is necessary and sufficient, but sibling readings (mitigation, adaptation) contest both the necessity and the sufficiency. The constraint passes the mandatrophy gate IF the reading's axiomatic claim (accumulation-driven material throughput is the driver; reduction is necessary) is accepted. It FAILS the gate if the alternative readings' axioms (decarbonization can decouple growth from impact; or adaptation is preferable to prevention) gain institutional authority. The measurement series show theater rising (0.28→0.42) while extractiveness also rises — this is NOT mandatrophy on its face (a piton is low-function high-theater), but it indicates that policy is mixing genuine transition work with enforcement theater, creating cover stories. The key mandatrophy question: is the constraint extracting in the name of coordination (tangled rope — some participants benefit, some pay, coordination is real), or is it extracting in the name of a failed coordination that has become pure power transfer (snare masquerading as rope, or piton theater)? The foundational axiom (material reduction is necessary) being contested by organized, credible voices (mitigation advocates) suggests the constraint may be hardening around a disputed premise — a scenario where enforcement intensity rises while problem-solution legitimacy declines, which is the signature of mandatrophy onset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_of_material_reduction,
    'Is material throughput reduction necessary to stay within planetary boundaries, or can decoupling (growth + reduced impact via efficiency/electrification/circular economy) achieve the same biophysical outcome?',
    'Empirical tracking of (1) whether decoupling scales globally, (2) whether efficiency gains are overwhelmed by consumption growth (Jevons paradox), (3) whether circular economy models can recycle high-volume material flows at full scale without degradation. Five-to-ten-year empirical windows on electric vehicle lifecycle emissions, renewable energy resource constraints, agricultural intensification impacts.',
    'If decoupling succeeds at scale, material reduction becomes unnecessary and degrowth reading loses its core justification; the constraint collapses to efficiency optimization (tangled_rope→rope). If decoupling fails (sufficiency still requires absolute reduction), the degrowth axiom is vindicated and the constraint''s framing is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_material_reduction, empirical, 'Core empirical contested premise: whether material reduction is necessary or whether technological efficiency + circular economy can achieve biophysical goals at current throughput.').

omega_variable(
    global_north_first_allocation_principle,
    'Is North-first material reduction (constrain Global North first, allocate remaining throughput to Global South development) the appropriate justice principle, or does it unfairly subordinate development rights of currently poor populations to the environmental consequences of prior Northern accumulation?',
    'Negotiation and legitimacy among Global South governments, development economists, and post-colonial scholars. Whether Global South coalitions accept North-first as fair recompense vs. reject it as colonial imposition. Whether alternative allocation principles (equal per-capita rights, needs-based allocation, historical responsibility scaled to current capacity) gain institutional standing.',
    'If Global South governments and civil society accept North-first, the constraint''s justice framing is validated and exclusion of their voices is covered by their subsequent acceptance. If they reject it and demand alternative allocation (or no constraint at all), the constraint becomes delegitimized as imposed extraction, reclassifying from tangled_rope (with contested coordination) to snare (with exposed victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_north_first_allocation_principle, empirical, 'Justice allocation question: whether North-first material constraint respects Global South development rights or subordinates them to Northern environmental goals.').

omega_variable(
    suppression_mechanism_structural_vs_identity,
    'Is the measured suppression (0.71) driven by structural barriers (capital reallocation, material scarcity, enforcement machinery) or internalized identity-fusion (consumption-as-identity, growth-as-culture, affluence-as-worth)?',
    'Post-transition empirical observation: do individuals who exit high-consumption systems carry the suppression with them (internalized), or does suppression dissolve once structural enforcement is removed? Do communities that voluntarily adopt sufficiency show lower post-exit psychological costs (structural suppression) or comparable costs (internalized)? Evidence from sufficiency-transition social movements and managed relocation studies.',
    'If suppression is primarily structural, enforcement removal or voluntary exit dissolves it — the constraint is reversible coercion. If internalized, people carry the suppression after exit, indicating the constraint has fused identity to consumption patterns — a deeper extraction mechanism. Higher internalization suggests the constraint operates at identity level and would require cultural/narrative work beyond material redistribution to undo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_identity, empirical, 'Suppression mechanism ambiguity: whether measured suppression in the consumption class is structural (external barriers, scarcity) or internalized (identity fusion, worth-through-consumption).').

omega_variable(
    theater_in_degrowth_policy,
    'What fraction of the rising theater (0.28→0.42) represents genuine transition infrastructure vs. performative measures that create appearance of reduction without delivering material throughput change?',
    'Forensic accounting of degrowth policy expenditure: renewable energy capacity actually displacing fossil fuels vs. renewable capacity added while fossil remains; regenerative agriculture land share vs. industrial agriculture land share; material recirculation rate in circular economy pilots vs. virgin material extraction. Tracking between policy announcements and actual material-flow changes.',
    'High theater fraction (>50%) indicates the constraint is increasingly performative — a snare with theatrical cover, or a piton wearing rope clothes. Low theater fraction (<20%) indicates policy is genuinely restructuring material flows. The measurement trajectory (theater rising while extractiveness plateaus) suggests theater is increasing relative to functional change — a signal of Goodhart drift (the enforcement metric is replacing the outcome metric).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_in_degrowth_policy, empirical, 'Policy theater drift: whether rising theater ratio reflects genuine transition infrastructure or performative appearance of reduction masking continued high throughput.').

omega_variable(
    kernel_contest_under_determination,
    'Is the contest between degrowth, mitigation, and adaptation readings a genuine structural disagreement about response (readings coexist, no foreclosure), or does one reading''s core axiom logically foreclose the others within a coherent framework?',
    'Logical analysis: does accepting ''material reduction is necessary'' (degrowth axiom) foreclose ''decoupling is sufficient'' (mitigation axiom)? Does accepting ''prevention is preferable'' (degrowth/mitigation shared) foreclose ''adaptation is the available response'' (adaptation axiom)? Or can all three be held simultaneously by a single coherent actor — e.g., ''we should do mitigation, but if that fails, we need adaptation, and if decoupling fails, we need degrowth as well''?',
    'If readings coexist (no foreclosure), the contest is about resource allocation and priority, not truth. If one forecloses others, the contest is about which reading correctly represents the problem — a factual contest where empirical resolution is possible. If under-determined (multiple coherent frameworks, each containing different readings), the choice between readings is framing-dependent, not fact-dependent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_under_determination, conceptual, 'Meta-contest about the kernel itself: whether the three readings are logically independent positions or whether some readings rule out others within coherent frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t8, climate_response_obligation__degrowth_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t16, climate_response_obligation__degrowth_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(clim_tr_t16, observed).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__degrowth_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(clim_tr_t24, projected).
narrative_ontology:measurement(clim_tr_t32, climate_response_obligation__degrowth_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(clim_tr_t32, projected).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__degrowth_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t8, climate_response_obligation__degrowth_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t16, climate_response_obligation__degrowth_reading, base_extractiveness, 16, 0.74).
narrative_ontology:measurement_basis(clim_be_t16, observed).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__degrowth_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement_basis(clim_be_t24, projected).
narrative_ontology:measurement(clim_be_t32, climate_response_obligation__degrowth_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement_basis(clim_be_t32, projected).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__degrowth_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t8, climate_response_obligation__degrowth_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t16, climate_response_obligation__degrowth_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(clim_su_t16, observed).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__degrowth_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(clim_su_t24, projected).
narrative_ontology:measurement(clim_su_t32, climate_response_obligation__degrowth_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(clim_su_t32, projected).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__degrowth_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(clim_su_t40, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(clim_grid_01, climate_response_obligation__degrowth_reading, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(clim_grid_02, climate_response_obligation__degrowth_reading, accessibility_collapse(class), 40, 0.68).
narrative_ontology:measurement(clim_grid_03, climate_response_obligation__degrowth_reading, accessibility_collapse(individual), 0, 0.45).
narrative_ontology:measurement(clim_grid_04, climate_response_obligation__degrowth_reading, accessibility_collapse(individual), 40, 0.72).
narrative_ontology:measurement(clim_grid_05, climate_response_obligation__degrowth_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(clim_grid_06, climate_response_obligation__degrowth_reading, accessibility_collapse(organizational), 40, 0.78).
narrative_ontology:measurement(clim_grid_07, climate_response_obligation__degrowth_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(clim_grid_08, climate_response_obligation__degrowth_reading, accessibility_collapse(structural), 40, 0.72).
narrative_ontology:measurement(clim_grid_09, climate_response_obligation__degrowth_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(clim_grid_10, climate_response_obligation__degrowth_reading, resistance(class), 40, 0.68).
narrative_ontology:measurement(clim_grid_11, climate_response_obligation__degrowth_reading, resistance(individual), 0, 0.65).
narrative_ontology:measurement(clim_grid_12, climate_response_obligation__degrowth_reading, resistance(individual), 40, 0.71).
narrative_ontology:measurement(clim_grid_13, climate_response_obligation__degrowth_reading, resistance(organizational), 0, 0.78).
narrative_ontology:measurement(clim_grid_14, climate_response_obligation__degrowth_reading, resistance(organizational), 40, 0.74).
narrative_ontology:measurement(clim_grid_15, climate_response_obligation__degrowth_reading, resistance(structural), 0, 0.81).
narrative_ontology:measurement(clim_grid_16, climate_response_obligation__degrowth_reading, resistance(structural), 40, 0.75).
narrative_ontology:measurement(clim_grid_17, climate_response_obligation__degrowth_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(clim_grid_18, climate_response_obligation__degrowth_reading, stakes_inflation(class), 40, 0.62).
narrative_ontology:measurement(clim_grid_19, climate_response_obligation__degrowth_reading, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(clim_grid_20, climate_response_obligation__degrowth_reading, stakes_inflation(individual), 40, 0.74).
narrative_ontology:measurement(clim_grid_21, climate_response_obligation__degrowth_reading, stakes_inflation(organizational), 0, 0.61).
narrative_ontology:measurement(clim_grid_22, climate_response_obligation__degrowth_reading, stakes_inflation(organizational), 40, 0.81).
narrative_ontology:measurement(clim_grid_23, climate_response_obligation__degrowth_reading, stakes_inflation(structural), 0, 0.71).
narrative_ontology:measurement(clim_grid_24, climate_response_obligation__degrowth_reading, stakes_inflation(structural), 40, 0.78).
narrative_ontology:measurement(clim_grid_25, climate_response_obligation__degrowth_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(clim_grid_26, climate_response_obligation__degrowth_reading, suppression(class), 40, 0.72).
narrative_ontology:measurement(clim_grid_27, climate_response_obligation__degrowth_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_28, climate_response_obligation__degrowth_reading, suppression(individual), 40, 0.68).
narrative_ontology:measurement(clim_grid_29, climate_response_obligation__degrowth_reading, suppression(organizational), 0, 0.64).
narrative_ontology:measurement(clim_grid_30, climate_response_obligation__degrowth_reading, suppression(organizational), 40, 0.76).
narrative_ontology:measurement(clim_grid_31, climate_response_obligation__degrowth_reading, suppression(structural), 0, 0.72).
narrative_ontology:measurement(clim_grid_32, climate_response_obligation__degrowth_reading, suppression(structural), 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% The climate_response_obligation kernel decomposes into three structurally distinct constraint readings. Degrowth reading prioritizes material throughput reduction and planetary biosphere as primary beneficiary; mitigation reading prioritizes decarbonization and intergenerational welfare via prevented warming; adaptation reading prioritizes resilience and managed adjustment to unavoidable warming. Each reading has different beneficiary/victim structures, different ε values (degrowth is highly extractive for Global North consumption class; mitigation is moderate; adaptation is moderate-to-low if resource-constrained), and different operational logics. The three readings are linked via network.affects_constraints because mitigation success would undermine degrowth necessity, adaptation infrastructure construction competes for resources with mitigation and degrowth transition, and the validity of any one reading depends partly on the empirical falsification of the others' core assumptions. This is a constraint family under kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
