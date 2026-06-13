% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Planned Economic Contraction as Legitimate Climate Response (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading of climate harm prevention asserts that legitimate
 *   climate response requires planned, deliberate economic contraction in the
 *   Global North as the only physically plausible and morally defensible path
 *   to preventing catastrophic warming. It rejects mitigation-within-growth
 *   frameworks as false solutions that delay effective action while obscuring
 *   the true costs being externalized onto the Global South and future
 *   generations. The reading institutes a constraint: planned Northern
 *   contraction becomes the legitimate boundary of climate policy, and
 *   growth-compatibility becomes illegitimate rather than the default
 *   assumption. Global North workers and consumers are the primary targets
 *   (bearing job loss, consumption reduction, asset write-downs); Global
 *   South populations and future generations are the primary beneficiaries
 *   (inheriting a less-degraded climate and reduced catastrophic risk). The
 *   constraint is claimed as tangled_rope (genuine coordination problem +
 *   asymmetric extraction) and the metrics describe substantial extraction
 *   and active enforcement requirements.
 *
 * KEY AGENTS:
 *   - global_north_present_workers: moderate power, biographical horizon, constrained exits — bear the employment and wage costs of deliberate economic contraction
 *   - global_north_present_consumers: moderate power, biographical horizon, constrained exits — bear the material consumption reductions required by contraction
 *   - global_south_populations: powerless, generational horizon, trapped exits — primary beneficiaries of reduced CO2 forcing and avoided catastrophic impacts
 *   - future_generations: powerless, civilizational horizon, trapped exits — ultimate beneficiaries whose habitability depends on present contraction enforcement
 *   - incumbent_carbon_intensive_industries: powerful, biographical horizon, constrained exits — face demand destruction and asset write-downs
 *   - degrowth_movement_advocates: organized, generational horizon, mobile exits — agenda-setters articulating and defending the reading
 *   - global_north_governments: institutional, generational horizon, constrained exits — excluded from the reading's beneficiary class but hold enforcement veto
 *   - ecological_science_consensus: analytical seat, universal scope — corroborates the carbon-budget exhaustion claim that grounds the reading's necessity frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Planned Economic Contraction as Legitimate Climate Response (Degrowth Reading)").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, 'ace7a09c-493b-4998-9b0d-dbb5b2af66b9').
narrative_ontology:cs_kernel_codification('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', distributed).
narrative_ontology:cs_authority_grounding('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', distributed).
narrative_ontology:cs_reading_relation('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', foundational, decoupling_physically_impossible).
narrative_ontology:cs_axiom_status(decoupling_physically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', decoupling_physically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', foundational, intergenerational_justice_requires_present_contraction).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_present_contraction, holdable).
narrative_ontology:cs_axiom_grounding('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', intergenerational_justice_requires_present_contraction, deontological).
narrative_ontology:cs_reference_frame('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', ecological_stability_framework).
narrative_ontology:cs_drift_state('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', contemporary_carbon_budget_exhaustion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ace7a09c-493b-4998-9b0d-dbb5b2af66b9', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_present_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_present_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, incumbent_carbon_intensive_industries).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, ecological_overshoot_thesis).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, decoupling_impossibility).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, intergenerational_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Employment and real wages depend on sustained production and consumption levels that degrowth reading frames as unsustainable. A deliberate contraction would eliminate jobs in carbon-intensive sectors (energy, automotive, aviation, construction) faster than alternative employment emerges. Exit options are constrained: emigration requires privilege; sector transition requires retraining with no guarantee of comparable wages or working conditions; organizing labor power in a contracting economy faces coordination and political challenges.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_present_workers, payer,
    moderate, biographical, constrained, global).

% Current consumption levels (housing size, transport access, material goods, energy use per capita) are normalized as baseline and defended as rights. Planned contraction would require visible, sustained reduction in material access — smaller housing, rationed energy, constrained mobility, reduced consumer choice. The constraint frames this as necessary; resistance runs high because the cost is distributed to present-day individuals while benefits accrue to Global South and future generations.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_present_consumers, payer,
    moderate, biographical, constrained, global).

% Bear the primary climate harms from Global North emissions (sea-level rise, agricultural collapse, water stress, extreme weather) while having minimal historical responsibility. The degrowth reading frames Northern contraction as restitution and mitigation simultaneously: reducing the atmospheric CO2 budget constraint that shadows all Global South development pathways. Benefits are existential (preserved habitability, reduced displacement, water security) but are delivered by others' constraints, not through direct agency.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Cannot participate in present consumption or negotiate the contraction's design. The degrowth reading frames rapid Northern contraction as the primary mechanism preserving their habitability and option set. The benefit is ultimate (a livable climate) but entirely dependent on present actors' enforcement discipline — future generations cannot enforce compliance with contraction, only inherit its consequences.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Fossil fuel extraction, automotive manufacturing, commercial aviation, cement and steel production derive economic viability from high-volume consumption enabled by growth. Planned contraction would eliminate demand for their primary products and force asset write-downs (stranded assets). They possess capital for lobbying and agenda-setting but are locked into current business models by infrastructure, workforce, and investor structures — transition is possible only through external enforcement, not through market signals.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, incumbent_carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Articulate and defend the degrowth reading: that ecological limits make planned contraction morally and physically necessary, and that growth-within-mitigation frames are legitimizing delay. Possess analytical credibility, institutional presence in academic and activist spaces, and rhetorical capacity to reframe contraction as justice rather than deprivation. Do not currently possess enforcement power but seek to establish the reading as authoritative within climate governance institutions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_movement_advocates, agenda_setter,
    organized, generational, mobile, global).

% Face electoral pressures from present-day Northern voters and are institutionally dependent on corporate tax bases. The degrowth reading's requirement for planned contraction conflicts directly with reelection and state capacity maintenance. Governments maintain formal commitment to 1.5°C climate goals but operationally prioritize mitigation-within-growth framings. They are structurally excluded from the degrowth reading's constituency (it frames Northern governments as the enforcement vehicle, not the primary beneficiary) but hold veto power over its implementation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_center_left_governments, excluded,
    institutional, generational, constrained, national).

% The degrowth reading invokes IPCC findings that carbon budgets for 1.5°C are nearly exhausted and that modeled pathways to 1.5°C require either rapid decarbonization (not yet demonstrated at scale) or negative emissions (unproven at necessary scale). Science does not prescribe policy but establishes constraint boundaries. The reading uses science as corroboration that growth-as-usual makes target-achievement impossible; science itself remains neutral on whether contraction is legitimate or the growth boundary should be abandoned.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, ecological_science_consensus, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__degrowth_reading, ecological_science_consensus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, degrowth_movement_advocates).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the remaining atmospheric carbon budget across nations and generations such that the Global South and future inhabitants inherit a habitable climate rather than a locked-in warming trajectory. Under degrowth framing, the coordination problem is not 'how do we decarbonize while growing' but 'how do we equitably distribute the burden of necessary contraction.' The constraint coordinates burden-sharing across time and geography.
% TRANSFER_FUNCTION: Transfers economic contraction costs (job elimination, consumption reduction, asset write-downs in carbon-intensive sectors) from Global South and future generations to Global North present workers and consumers. Simultaneously transfers atmospheric habitability (reduced CO2 forcing, preserved rainfall regimes, avoided sea-level rise impacts) from Global South to the beneficiary classes. The transfer is asymmetric: Northern present actors bear tangible, near-term costs; Southern and future beneficiaries receive existential benefits but exercise no control over enforcement.
% ABSENT_VOICES: Global North labor unions (split on contraction support), precarious workers without organizational capacity, indigenous communities in Global South whose land and sovereignty are threatened but who are not formal parties to climate governance, and future generations themselves (by definition absent from present deliberation). The degrowth reading claims to speak for Global South and future interests but does so without their direct participation in the constraint's design or enforcement. This absence is contested — some Global South climate justice movements embrace the frame, while others reject it as Northern imposition that sidelines development rights.
% DISAPPEARANCE_RATIONALE: If the constraint (planned, deliberate, coordinated Northern contraction as legitimate climate response) disappeared, the trajectory would reorganize toward unconstrained pursuit of mitigation-within-growth or acceptance of adaptation-only frameworks. The absence of the contraction frame would not prevent climate change but would remove the primary narrative pressure pushing Northern governments toward accepting GDP reduction as legitimate. The world would rearrange toward acceptance of higher warming targets and emphasis on Global South adaptation rather than Northern mitigation burdens.
% FOUNDING_PROBLEM: The foundational claim is that ecological overshoot and carbon budget exhaustion make decoupled decarbonization (emissions reduction without GDP reduction) physically impossible at the required speed, and that any growth-compatible pathway to 1.5°C relies on speculative negative-emissions technologies and/or accepts climate targets of 2°C or higher, thereby abandoning the Global South and future generations to unlivable conditions.
% FOUNDING_PROBLEM_CORROBORATION: IPCC reports (AR6 WG3) document that pathways to 1.5°C require 'rapid, deep, and sustained reductions in greenhouse gas emissions' with scenarios showing limited overshoot or no overshoot requiring net-zero CO2 by 2050 — corroboration from outside the degrowth advocacy movement. However, the claim that this necessitates 'planned economic contraction' rather than decarbonized growth is contested: mainstream climate economics (Nordhaus, Stern, IEA) argues decarbonization is achievable within growth frameworks through technology substitution and carbon pricing. The founding problem's status hinges on the empirical claim about decoupling possibility, which is the subject of active scientific and economic dispute. Degrowth advocates cite peak-oil, material-limits, and rebound-effect literature; mainstream sources cite renewable cost curves and efficiency gains. Both cite peer-reviewed evidence; corroboration is thus partial and contested.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval because the constraint's political and enforcement machinery strengthens (carbon budgets tighten, climate impacts accelerate, the degrowth frame spreads in governance discourse). The base extractiveness of 0.68 reflects that beneficiaries gain existential habitability while payers lose present material access and employment — an asymmetry that cannot be resolved through Pareto-improving side payments because future generations and distant populations cannot negotiate. Suppression is high and rising (0.52 to 0.72) because the constraint's persistence depends on actively suppressing alternatives: growth-compatible mitigation narratives must be delegitimized, fossil-fuel industry political power must be contained, labor concerns about unemployment must be reframed as 'just transition' rather than recognized as material loss. Theater is moderate (0.41 at interval end) because some of the enforcement activity is genuine infrastructure transformation and policy redesign, but a rising share is defensive framing — justifying contraction as necessary rather than changing actual material conditions. Accessibility collapse is relatively low (0.52) because alternatives remain rhetorically available (adaptation-only, mitigation-within-growth) and are defended by powerful actors; the constraint must work to foreclose alternatives rather than being an inevitable natural limit. Resistance is high (0.78) because payers have substantial agency and leverage (labor mobilization, consumer purchasing power, incumbent industry capital) and are defending their material interests. The measurement series captures the trajectory of the reading's establishment as a governance frame: early adoption in academic and activist circles (low extractiveness), gradual penetration into policy discourse and regulatory language (middle interval rising), and stabilization as the constraint's enforcement machinery matures (plateau at interval end).
 *
 * PERSPECTIVAL GAP:
 *   The degrowth_movement_advocates and ecological_science_consensus seats should compute as beneficiaries (they define the reading and invoke legitimacy through science corroboration), but their power is primarily rhetorical/analytical rather than material — they do not extract revenue or gain employment from the constraint's operation. Global_north_governments compute as excluded observers rather than agenda-setters because the reading frames them as the enforcement vehicle for beneficiaries they do not represent. From the government's seat, the constraint is experienced as a political liability (imposing costs on their electoral base) rather than a legitimate articulation of their authority; from the degrowth seat, governments are the only actors with sufficient power to enforce the constraint but are structurally captured by Northern capital. This asymmetry — government as necessary-but-hostile enforcer — is the constraint's core political instability. Global_north_workers compute as high-d targets, but their resistance to the constraint is read differently depending on the observer: as justified labor defense (by labor unions and left critics) or as selfish obstruction of necessary climate action (by degrowth advocates and Global South speakers). The divergence is not resolved within a single seat but rather reflects the distribution of blame for the constraint's necessary extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South and future generations are full beneficiaries (d ≈ 0.0–0.2): they receive existential climate benefits without running the constraint and with zero ability to avoid it. Global North present workers and consumers are full targets (d ≈ 0.8–1.0): they bear concentrated employment and consumption costs, have exit options only at the cost of migration or underground-economy work, and did not design the constraint. Incumbent carbon industries are targets (d ≈ 0.9): they face demand destruction and asset write-downs without receiving coordination benefits. Degrowth advocates are partial beneficiaries (d ≈ 0.1–0.3): they gain rhetorical legitimacy and policy influence but do not directly capture the constraint's material flows; they also bear reputational costs from being associated with job loss and consumption reduction. Global North governments sit at d ≈ 0.5–0.6 (symmetric, conflicted): they must enforce the constraint to maintain climate credibility but face electoral punishment from Northern workers and consumers; they gain international legitimacy with Global South actors but lose domestic political capacity. The directionality derivation from beneficiary/victim declarations runs straightforward for most seats; no overrides are necessary because the structural data clearly map to the derived directions.
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading avoids a common mandatrophy trap: the constraint is justified by reference to a founding problem (carbon budget exhaustion, overshoot) that is still live and will remain live across the interval (the carbon budget deficit grows, not shrinks, as atmospheric CO2 accumulates). The constraint's mandate (deliberate contraction as legitimate response) is not decoupled from the founding problem; rather, the problem's persistence would deepen the constraint's claimed necessity. However, a secondary mandatrophy risk exists: if technology breakthroughs (fusion, direct air capture, synthetic biology for carbon sequestration) dramatically lower the cost of decarbonization, the founding problem's frame — 'decoupling is impossible' — would be directly contradicted, and the constraint would face the claim that its mandate has been superseded by new conditions. The degrowth reading is exposed to factual refutation if the empirical claim about decoupling possibility is falsified. The reading does not resolve this by denying the possibility of technological change, but rather by arguing that the timeline for such breakthroughs is longer than the remaining carbon budget allows — a quantitative trade-off claim that is itself contestable. The constraint's internal consistency (tangled_rope framing) is sound: it coordinates burden-sharing (genuine coordination function) and extracts from Northern payers (asymmetric extraction). The mandatrophy risk is not internal inconsistency but rather external falsification of the empirical grounding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_empirical_falsifiability,
    'Can greenhouse gas emissions be substantially decoupled from GDP growth through technology and efficiency improvements, or is decoupling a physical impossibility once material throughput is accounted for (absolute decoupling)?',
    'Long-term empirical tracking of absolute decoupling (emissions per unit of global GDP) at required pace (5–7% annual reductions through 2050) versus technological capability to deliver renewable electricity, electrified transport, and decarbonized industry at that scale. The IPCC AR6 pathways are the primary arbiter: do the 1.5°C-compatible scenarios actually achieve decoupling, or do they rely on speculative negative-emissions technologies?',
    'If absolute decoupling proves empirically possible at the required scale and speed, the founding problem of the degrowth reading is falsified, and the constraint would be reclassified as extractive without genuine coordination justification (a snare riding on a false claim). If decoupling remains impossible, the founding problem is corroborated and the constraint''s necessity frame is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_empirical_falsifiability, empirical, 'Whether decarbonization without growth-reduction is physically achievable at the required scale and speed.').

omega_variable(
    intergenerational_legitimacy_framing,
    'Is it legitimate for present actors to impose constraints on present-day Northern workers and consumers (who did not individually choose the carbon-intensive infrastructure they depend on) in order to benefit future generations and distant populations (who have no voice in the design)? Or does the constraint impose duties on present actors to future others that exceed the justifiable bounds of political obligation?',
    'Philosophical and political-theoretical debate over intergenerational justice frameworks (Rawls, Sen, critical theory perspectives on obligation and consent). No empirical resolution; contestation within different normative frameworks about the standing of future and distant beneficiaries to ground present costs.',
    'If intergenerational obligation is accepted as legitimate grounding, the constraint''s asymmetry (present costs, future benefits) is justified and the type classification holds. If intergenerational obligations are rejected or limited, the constraint becomes purely extractive (one present group imposing costs on another present group without consent) and would be reclassified as snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_legitimacy_framing, preference, 'Whether present actors can legitimately be bound by duties to future and distant beneficiaries.').

omega_variable(
    global_north_working_class_position,
    'Are Global North workers and consumers the primary targets of contraction costs, or are they potential beneficiaries of a just transition that decouples employment security and material access from growth? Does the constraint necessitate unemployment and impoverishment, or can contraction be designed to preserve or improve working-class welfare?',
    'Historical and comparative analysis of economic contraction periods (post-industrial decline, war economies, planned transitions) tracking working-class welfare outcomes. Modeling of just-transition scenarios with public employment guarantees, retraining, and income support to test whether contraction can be decoupled from unemployment.',
    'If contraction necessitates unemployment and material loss for workers, they remain full targets (d ≈ 0.9) and the constraint is extractive as classified. If contraction can be redesigned to protect working-class welfare through public investment and full-employment guarantees, workers become partial beneficiaries (d ≈ 0.5) and the constraint''s character shifts toward genuine coordination (all parties benefit, costs are distributed differently).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_north_working_class_position, conceptual, 'Whether contraction is structurally coupled to unemployment and immiseration or can be decoupled through policy design.').

omega_variable(
    southern_agency_and_voice,
    'Does the degrowth reading legitimately represent Global South interests and agency, or does it impose a Northern frame (contraction as salvation) onto Southern populations without genuine participation in the constraint''s design and justification?',
    'Comparative analysis of Global South voices in degrowth discourse: what proportion of degrowth movement leadership and agenda-setting is Global South-based versus Northern-based? What do Global South climate movements (as distinct from development actors) prioritize: Northern contraction, Southern development rights, or other frames?',
    'If Global South participation in defining the reading is minimal and the frame is imposed, the constraint gains a secondary extraction dynamic (Northern advocates extracting rhetorical authority by claiming to speak for the South without being authorized). If Global South actors drive the degrowth frame autonomously, the constraint''s legitimacy is strengthened. If Global South priorities diverge from degrowth (prioritizing development rights and just transition over Northern contraction), the constraint''s beneficiary claim is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(southern_agency_and_voice, empirical, 'Whether the degrowth reading represents authentic Global South voice or Northern projection onto the South.').

omega_variable(
    enforcement_feasibility_under_democracy,
    'Can planned contraction be enforced through democratic institutions when it imposes material costs on the electoral majority (Global North voters) and when incumbent industries and opposed political movements possess substantial mobilization capacity?',
    'Political-economy analysis of enforcement feasibility: tracking acceptance of contraction-related policies (carbon taxes, emissions caps, industrial closures) by democratic publics; analyzing whether any democratic government has sustained a contraction program over an electoral cycle; examining whether contraction requires authoritarian or emergency governance structures.',
    'If enforcement requires suspension of democratic accountability, the constraint becomes politically illegitimate (gains mandatrophy risk: the mandate requires coercive enforcement that violates the principles the reading claims to defend). If enforcement is compatible with democracy, the constraint''s internal consistency holds. If neither route is viable, the constraint remains aspirational but unimplementable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_feasibility_under_democracy, empirical, 'Whether planned contraction can be enforced through democratic institutions or requires authoritarian/emergency measures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__degrowth_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__degrowth_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__degrowth_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__degrowth_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__degrowth_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__degrowth_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__degrowth_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__degrowth_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__degrowth_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__degrowth_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__degrowth_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__degrowth_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__degrowth_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__degrowth_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__degrowth_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__degrowth_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__degrowth_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__degrowth_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__degrowth_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__degrowth_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__degrowth_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, carbon_budget_constraint).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, just_transition_labor_framework).

% DUAL FORMULATION NOTE:
% This constraint is ONE READING of the climate_harm_prevention kernel. Sibling readings (mitigation_priority, adaptation_priority) are separate constraint stories with different ε values, beneficiary/victim structures, and justifications. They share the kernel (climate harm is real, responses must be grounded) but differ in the allocation principle and legitimacy frame. Each reading instantiates a distinct constraint; the network edges link the family members. The degrowth reading forecloses the claim that decarbonization is possible without growth-reduction and influences (but does not foreclose) adaptation-priority by narrowing the acceptable warming budget.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
