% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Climate Response Obligation (Degrowth Reading): Reduce Material Throughput for Planetary Boundaries
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading frames climate response as requiring absolute
 *   reduction in material throughput (extraction, processing, transport,
 *   disposal) to stay within biophysically determined planetary boundaries.
 *   This reading treats sufficiency (meeting needs, not wants) as the
 *   operative principle rather than efficiency (producing more with less
 *   material). It positions Global North high-consumption populations and
 *   capital-accumulation-dependent actors as the primary extractive mechanism
 *   — their consumption and production patterns impose extraction pressure
 *   that future generations and ecosystem stability bear the cost of. Unlike
 *   mitigation-priority reading (rapid decarbonization compatible with
 *   growth) and adaptation-priority reading (invest in resilience to
 *   inevitable warming), degrowth reading treats material throughput as the
 *   decision variable, not technological decarbonization or adaptive
 *   capacity. The constraint is claimed as tangled_rope because it
 *   coordinates allocation of planetary absorption capacity while
 *   asymmetrically extracting from high-consumption actors and
 *   growth-dependent institutions. This is one reading of the contested
 *   climate-response-obligation kernel; the others (mitigation and
 *   adaptation) remain live policy positions held by different institutional
 *   factions.
 *
 * KEY AGENTS:
 *   - Planetary biophysical systems (non-agent beneficiary): soil carbon, freshwater, ocean absorption capacity, biodiversity
 *   - Future generations (non-agent beneficiary): inherit lower cumulative extraction, lower temperature rise, lower systemic fragility
 *   - Global North high-consumption populations (powerful payer): must reduce material throughput from 20–30 tons/capita to 5–8 tons/capita; constrained exit (cannot consume less without lifestyle reduction)
 *   - Capital accumulation actors (institutional payer): business models locked into throughput growth; exit requires institutional dissolution
 *   - Incumbent growth-industrial complex (institutional payer): mining, fossil fuels, mass manufacturing calibrated to high-throughput production
 *   - Global South development populations (moderate beneficiary/payer): gain development space if North reduces; constrained if North does not
 *   - Degrowth movement advocates (agenda-setter): define throughput reduction as binding, sufficiency as operative mechanism
 *   - Ecological economists and researchers (observers): measure biophysical constraints, track feasibility of degrowth transition
 *   - Adaptation and mitigation-priority advocates (excluded): their framing treats throughput reduction as unnecessary; degrowth reading forecloses their strategic options
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.71).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Climate Response Obligation (Degrowth Reading): Reduce Material Throughput for Planetary Boundaries").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '842ed8c8-d9c1-4fad-969c-5b8e9a87bff2').
narrative_ontology:cs_kernel_codification('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', fixed_text).
narrative_ontology:cs_authority_grounding('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', expertise).
narrative_ontology:cs_interpretation_layer_present('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2').
narrative_ontology:cs_reading_relation('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', foundational, planetary_boundaries_absolute_and_binding).
narrative_ontology:cs_axiom_status(planetary_boundaries_absolute_and_binding, holdable).
narrative_ontology:cs_axiom_grounding('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', planetary_boundaries_absolute_and_binding, empirically_contingent).
narrative_ontology:cs_axiom('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', foundational, material_throughput_reduction_necessary).
narrative_ontology:cs_axiom_status(material_throughput_reduction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', material_throughput_reduction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', secondary, rebound_effects_offset_efficiency).
narrative_ontology:cs_axiom_status(rebound_effects_offset_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', rebound_effects_offset_efficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', pre_transgression_biophysical_stability).
narrative_ontology:cs_drift_state('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', contemporary_anthropocene_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('842ed8c8-d9c1-4fad-969c-5b8e9a87bff2', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_biophysical_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, vulnerable_ecosystems).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_high_consumption_populations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, capital_accumulation_dependent_actors).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, incumbent_growth_industrial_complex).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_development_aspiring_populations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_development_aspiring_populations).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_biophysical_necessity).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, intergenerational_justice_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, sufficiency_economics_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The biophysical substrate (soil carbon, freshwater aquifers, ocean absorption capacity, biodiversity, nutrient cycling, climate stability) that underpins all economic activity. Under degrowth reading, material throughput reduction directly reduces extraction pressure on soil carbon, freshwater aquifers, fisheries, atmospheric carbon budget, and ecosystem integrity. Reduced throughput means reduced degradation and increased possibility of system recovery and stabilization.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_biophysical_systems, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_biophysical_systems).

% Generations born after the degrowth constraint take effect inherit a planetary system with lower cumulative carbon, lower temperature rise, lower biodiversity loss, lower industrial-waste burden, and higher systemic resilience than under high-throughput trajectories. They cannot voice preferences today; the constraint frames their interest as overriding present-consumption preferences.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, future_generations).

% Citizens and households in high-income countries (North America, Western Europe, East Asia, Australia) whose material consumption rates (20–30 tons material per capita annually) drive approximately 70% of global material extraction while comprising 15% of global population. Under degrowth reading, their material throughput must fall to 5–8 tons per capita to stay within planetary boundaries while allowing Global South development. This requires reducing housing floor space per capita (downsizing from 100m² to 50–60m²), transportation energy (shift from personal vehicles to public transit), food waste (eat more locally, less meat), product replacement cycles (wear clothing longer, repair rather than replace electronics), and industrial production. Exit options are constrained: they cannot move material consumption pressure to other planets, cannot outsource the biophysical limit through outsourced manufacturing (the limit is on total extraction, not location), and cannot maintain present living standards while reducing material throughput below planetary-boundary-compliant levels.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_high_consumption_populations, payer,
    powerful, biographical, constrained, global).

% Corporations, investment firms, asset managers, and financial institutions whose business models and fiduciary mandates mandate continuous material throughput growth to service debt repayment and achieve shareholder returns. Under degrowth reading, their core operational premise (compound annual growth in material extraction and production) becomes extractive rather than virtuous and is framed as institutionally locked into planetary boundary violation. Exit would require reconstituting business models around throughput reduction, accepting lower or zero returns, and rewriting fiduciary mandates from shareholder maximization to stakeholder welfare. Most capital-dependent institutions cannot undertake this transformation without institutional dissolution or merger into fundamentally different organizational forms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, capital_accumulation_dependent_actors, payer,
    institutional, biographical, identity_locked, global).

% Mining, fossil fuel extraction, petrochemicals, mass manufacturing, industrial agriculture, and logistics industries whose production capacity, supply chains, workforce expertise, and infrastructure investment are calibrated to high-throughput production. Degrowth reduction to planetary boundaries would strand much of this infrastructure (mines, refineries, factories, distribution centers); require workforce reallocation toward repair, remanufacturing, and transition-support sectors; and eliminate many industrial supply chains optimized for high-volume, low-cost production. The institutional identity of these sectors is inseparable from growth production and throughput expansion; exit means the industry's dissolution as currently constituted and reconstitution in fundamentally different form.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, incumbent_growth_industrial_complex, payer,
    institutional, biographical, identity_locked, global).

% Countries and populations in Global South whose material consumption per capita (2–6 tons) is below planetary-boundary-compliant levels but whose populations aspire to higher living standards, improved housing, transportation access, and industrial development. Under degrowth reading, their development path is constrained until Global North reduces material throughput, because planetary boundaries are absolute (can be divided but not exceeded globally) and cumulative (past extraction still affects atmospheric carbon). They benefit from the constraint's pressure on North consumption (creating development space) but pay through constrained industrial development opportunities, reduced capital inflows for fossil-fuel and high-throughput infrastructure projects, and structural dependence on North-led degrowth actually occurring. If Global North does not reduce, the constraint becomes a cap on their development and a perpetuation of North-South inequality.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_development_aspiring_populations, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_development_aspiring_populations, payer).

% Environmental movements, degrowth economists, climate justice organizations, indigenous-led land-rights advocates, and policy advocates who frame the constraint as necessary and morally warranted and view sufficiency-living and material reduction as solutions rather than sacrifices. They set the reading's agenda by defining planetary boundaries as binding, material throughput as the primary decision variable, and reduction as the operative enforcement mechanism. They organize policy campaigns, public discourse, legislative advocacy, and institutional pressure to enforce the reading's prioritization over growth-compatible alternatives. They gain leverage and institutional voice as the constraint gains political salience.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_movement_advocates, agenda_setter,
    organized, generational, mobile, global).

% Researchers working on material flow analysis, planetary boundaries science, biophysical accounting, ecological economics, and sufficiency economics. They observe and measure the biophysical constraints, quantify material throughput per capita, track Jevons rebound effects, and conduct research on the technical feasibility and social viability of degrowth transitions. Their role is to produce evidence on whether the constraint's premises hold and whether proposed enforcement mechanisms would actually achieve stated aims.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, ecological_economists_and_researchers, observer,
    analytical, generational, analytical, global).

% Technology optimists, clean-energy entrepreneurs, climate economists, and institutional actors who prioritize rapid decarbonization-with-growth (mitigation reading) as the binding response, treating carbon intensity rather than material throughput as the decision variable. They are structurally excluded from the degrowth reading's policy conversation because that reading's core premise (planetary boundaries are primary, material throughput must decrease absolutely) forecloses growth-compatible decarbonization as a viable response to climate crisis. They argue technological solutions (renewable energy, electric vehicles, green manufacturing) can provide development and consumption growth while decoupling from carbon and material throughput; degrowth reading treats this argument as empirically falsified (rebound effects, continued resource extraction) and institutionally complicit in extraction continuation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mitigation_priority_advocates, excluded,
    organized, generational, constrained, global).

% Climate adaptation advocates, resilience planners, and institutional actors who prioritize climate adaptation investment (adaptation reading) as the binding response, treating warming acceptance and resilience capacity as the decision variables rather than prevention or throughput reduction. They argue 2–3°C warming is already locked in by committed emissions; prevention efforts are economically costly relative to adaptive benefits; resources should be redirected toward resilience, relocation, and managed retreat in vulnerable regions. They are structurally excluded from degrowth reading's framing because degrowth reading treats mitigation (prevention) as possible and necessary if throughput reduces, and treats adaptation-acceptance as a cover story for preserving high-extraction, high-risk paths.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, adaptation_priority_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the biophysically limited planetary absorption and regeneration capacity (carbon budget, freshwater recharge, biodiversity intactness, soil health, nutrient cycling) among present consumption, future preservation, and ecological system stability. The coordination problem: how to prevent catastrophic climate feedback, systemic biodiversity collapse, and cascading ecological failure while allowing some actors development space and some regions livelihood improvement. Degrowth reading solves it by making material throughput reduction the coordination variable and distributing reduction burden (via sufficiency norms, consumption caps, industrial capacity reduction) inversely to per-capita current consumption: Global North reduces most (from 20–30 to 5–8 tons/capita); Global South reduces least (maintains or modestly increases from 2–6 toward 8 tons/capita); industrial sectors dependent on high-throughput production face capacity reduction and workforce reallocation.
% TRANSFER_FUNCTION: Transfers reduced consumption utility and material standard-of-living from Global North high-consumption populations to future generations and biophysical systems (via reduced extraction pressure, lower cumulative carbon, lower biodiversity loss, lower systemic fragility). Transfers development opportunity space from Global North industrial production capacity and capital investment toward Global South livelihood-economy development. Transfers labor time from high-throughput production (manufacturing, resource extraction, distribution of disposable goods) toward care work, repair and remanufacturing, ecological restoration, community provisioning, and transition-support sectors. Transfers institutional authority from growth-dependent corporations and financial institutions toward degrowth movements, ecological economists, and Global South development advocates.
% ABSENT_VOICES: Incumbent industrial actors (mining, fossil fuels, mass manufacturing) and growth-dependent financial institutions are structurally excluded from setting the reading's premises; they would argue for technological fixes (green manufacturing, renewable energy), efficiency improvements, and relocation of high-throughput production to lower-cost regions rather than absolute throughput reduction. Capital-accumulation-dependent actors frame the constraint as threatening their fiduciary mandates and institutional identity; their voices advocate for growth-compatible transition pathways rather than institutional reconstitution. Global South populations who aspire to North-style material consumption patterns (higher incomes, automobile ownership, large housing) are present as constrained beneficiaries but their preference for unconstrained development is not centered in degrowth framing (degrowth movements center equity but do not center Global South aspiration-driven consumption); actors seeking capital-intensive industrialization and outsourced manufacturing are excluded from the reading's design.
% DISAPPEARANCE_RATIONALE: If the degrowth constraint and its enforcement (material throughput caps, carbon budgets, industrial capacity reduction, sufficiency norms, lifecycle-extended product standards) disappeared overnight, material extraction would resume its pre-constraint trajectory; capital accumulation would re-accelerate; Global North consumption would rebound toward pre-transition levels; fossil-fuel and high-throughput industrial production would expand; planetary boundary transgression would continue and accelerate (current trajectory: ~1.5x planetary boundaries by 2050). The economic restructuring toward repair, remanufacturing, and care sectors would reverse as capital reinvests in high-margin, high-throughput production. The constraint's disappearance would materially rearrange the world toward higher-extraction, higher-carbon pathways and would foreclose the development space that Global South populations gained under the constraint. The world would not remain roughly unchanged; it would reorganize at scale toward extraction maximization.
% FOUNDING_PROBLEM: Climate destabilization (2°C+ warming locked in by present and committed emissions; 3–4°C committed within current infrastructure) and biodiversity collapse (sixth extinction event driven by habitat loss, resource extraction, and toxification) are products of material throughput rates that persistently exceed planetary regeneration and absorption capacity. The biophysical constraint is absolute and non-negotiable: carbon cannot be relocated to other planets; soil degradation cannot be industrialized away; species extinction is irreversible; ocean acidification is thermodynamically determined by atmospheric carbon. Technological efficiency improvements have systematically been offset by Jevons rebound effects and induced demand, leaving material throughput unchanged or rising despite decades of efficiency rhetoric. The founding problem: how to provide dignified livelihood and development opportunity for all humans within planetary boundaries that are non-negotiable and increasingly transgressed.
% FOUNDING_PROBLEM_CORROBORATION: Planetary Boundaries science (Rockström et al. 2009, 2015; Steffen et al. 2015; Earth Commission 2023) establishes biophysical thresholds as empirically measured facts. IPCC Sixth Assessment Reports (AR6, 2021–2023) confirm climate and biodiversity systems are degrading under current trajectories and warming beyond 1.5°C is now unavoidable. Global Material Extraction data (UN IRP, Exiobase) shows continued acceleration of material throughput (2009–2024: +40% extraction despite efficiency gains). Ecological economists (Herman Daly, Kate Raworth, Matthias Schmelzer) and climate scientists outside growth-dependent policy mainstream (Kevin Anderson, Kate Marvel, Stefan Rahmstorf) atttest that the founding problem is live and that throughput reduction is necessary. Land-use scientists (David Tilman, Johan Rockström) confirm biodiversity loss is driven by habitat conversion and resource extraction, not just climate. Capital-dependent institutions and growth-oriented economists attest the problem is overstated, solvable through efficiency and green technology, and that degrowth is economically damaging; their attestation is self-interested (they benefit from continued growth-compatible framing) and is not corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).

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
 *   Extractiveness is measured at 0.68 end-state because the degrowth constraint transfers consumption utility and material standard-of-living from Global North high-consumption populations (who currently appropriate ~70% of planetary regeneration capacity) to future generations and biophysical systems. This is not a voluntary transfer; it is enforced through material throughput caps, carbon budgets, and reduced industrial capacity. The transfer is irreversible within biographical time horizons (Global North populations cannot relocate consumption). Suppression is high (0.71) because the constraint's persistence depends on actively resisting incumbent industrial capacity expansion, blocking capital accumulation in high-throughput sectors, and enforcing sufficiency norms despite cultural pressure toward consumption growth in Global North and aspiration-driven demand in Global South. Theater ratio is moderate (0.42) and rising: early phases show substantive material throughput reduction in pilot regions (observed basis, 0.20–0.34 theater through t=15); later phases accumulate more carbon-accounting theater as decarbonization-with-growth narratives capture policy discourse and 'degrowth-compatible' production metrics obscure ongoing accumulation (projected basis, plateau at 0.42). The measurement series trace the constraint's evolution from early substantive enforcement toward later performative maintenance as political pressure builds against disruption costs and incumbent actors invest in rhetorical compliance (net-zero frameworks, green growth, circular economy) that preserve accumulation while claiming alignment with degrowth aims.
 *
 * PERSPECTIVAL GAP:
 *   Huge divergence between agenda-setter and payer seats. Degrowth advocates (organized agenda-setter, moderate power, mobile exit) experience the constraint as a necessary and morally warranted response to biophysical limits and view it as liberating (reduction of productivist pressure, recovery of time for care and community). Global North high-consumption populations (powerful payer, constrained exit) experience it as coercive lifestyle reduction and deprivation (loss of consumption choice, shrinking housing, reduced transportation). Capital-dependent actors (institutional payer, identity-locked exit) experience it as institutional threat requiring active resistance and rhetorical capture. Future generations and biophysical systems (non-agent beneficiaries) cannot experience it directly; the constraint frames their interest as overriding present-actor preferences. The engine should compute the agenda-setter seat as rope-aligned (they benefit from the constraint's symbolic and material enforcement) and the payer seats as snare or tangled-rope-aligned depending on directionality: high-consumption populations face high d (target end, asymmetric extraction); capital actors face identity-lock-mediated extraction (organized resistance possible but institutional dissolution cost is existential).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: planetary systems and future generations → d = 0.0 (full beneficiary, though non-agent beneficiaries cannot exercise exit). Degrowth advocates → d = 0.15 (beneficiary seat, mobile exit, moderate power, gain leverage and institutional voice from constraint enforcement; not the primary beneficiary but benefit from constraint's vindication). Payer directionality: Global North high-consumption populations → d = 0.88 (full target, powerful but constrained exit, material throughput reduction is non-negotiable under the constraint, cannot exit the planet). Capital-accumulation actors → d = 0.85 (full target, institutional power but identity-locked exit, business-model dissolution cost is existential, cannot pivot without losing institutional identity). Global South development populations → d = 0.55 (symmetric/ambiguous, benefit from North reduction but pay through constrained development; moderate power, constrained exit, generational time horizon means some development space exists but is contingent on North compliance). Adaptation/mitigation advocates → excluded role, not computed (d calculation not applicable to excluded stakeholders). Directionality overrides not needed: the derived chain (beneficiary/victim + exit → d) produces structurally sound differentiation without adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading does NOT exhibit mandatrophy at present. The founding problem (material throughput exceeds planetary regeneration capacity) is live and unresolved; planetary boundaries data continuously updates the problem's reality; material flow measurements track ongoing transgression. The constraint's mandate (reduce throughput to stay within boundaries) remains matched to the founding problem's persistence. However, a mandatrophy risk exists at high theater ratios (t>20): as rhetorical capture by growth-compatible frameworks (net-zero, green growth, circular economy) increases, the constraint's nominal enforcement may diverge from actual throughput reduction. If theater ratio approaches 0.6+ while measured material extraction plateaus or rises, mandatrophy would be declared (mandate persists rhetorically but founding problem's actual resolution is abandoned). This risk is documented in the measurement trajectory and in omega variables below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rebound_effect_magnitude,
    'Do efficiency gains in production and consumption reliably decouple material throughput from economic activity, or do Jevons rebound effects and induced demand consistently erode efficiency gains, leaving throughput unchanged or rising?',
    'Historical material flow data (1970–2024) compared across sectors and regions; controlled studies of consumption behavior after efficiency improvements; meta-analysis of rebound effect magnitudes in empirical literature.',
    'If rebound is substantial (>50% offset) and structural, degrowth reading''s claim that efficiency cannot solve throughput reduction is supported and the constraint''s enforcement focus on absolute reduction (not efficiency) is justified. If rebound is small (<20%) or sector-dependent, efficiency-with-growth reading gains credibility and degrowth reading''s extraction measure from high-consumption populations may be overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rebound_effect_magnitude, empirical, 'Whether technological efficiency can decouple material throughput from economic growth or whether rebound effects dominate.').

omega_variable(
    global_south_development_path_constraint,
    'Is Global South development legitimately constrained by planetary boundaries until Global North reduces, or is this constraint an illegitimate barrier to development equity and does it encode Global North privilege?',
    'Per-capita planetary boundary allocation frameworks (equal allocation vs. historical responsibility allocation vs. development-needs allocation); negotiation outcomes in climate and biodiversity agreements; political economy analysis of capital flow changes under different allocation rules.',
    'If development constraint is legitimate (planetary boundaries are absolute), degrowth reading''s constraint on Global South is justified as equitable burden-sharing. If constraint is seen as illegitimate barrier, the reading''s claim to intergenerational justice is contradicted by present-generational injustice; Global South victims set expands and bargaining power shifts toward forced-growth alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_development_path_constraint, preference, 'Whether constraining Global South development is a justified equitable response or an unjust preservation of North privilege.').

omega_variable(
    sufficiency_living_standard_social_viability,
    'Can sufficiency-living standards (5–8 tons material/capita; reduced consumption; extended product lifespans; local provisioning) be achieved at scale without social collapse, mental-health crises, or political instability in Global North populations whose identity and status are culturally fused to consumption?',
    'Pilot degrowth transitions (Costa Rica material throughput reduction, Transition Town movements, cooperative housing models); psychological research on identity-dissolution and consumption-identity fusion; political polling and protest response to actual consumption restrictions; comparative analysis of stability outcomes under degrowth vs. growth-constrained recession.',
    'If viability is high (identity-fusion is culturally malleable, sufficiency is experientially acceptable), degrowth constraint is implementable. If viability is low (identity-fusion is deep, transition causes widespread instability), the constraint''s enforcement cost becomes prohibitive and suppression requirement may rise sharply; constraint may shift toward piton-like performative maintenance as political cover for actual bifurcation (sufficiency-lite for North, growth-constrained for South, planetary boundaries breached through hybrid instability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_living_standard_social_viability, empirical, 'Whether sufficiency-living standards can be achieved and sustained at scale in high-consumption societies.').

omega_variable(
    capital_accumulation_vs_degrowth_incompatibility,
    'Is capital accumulation (compound growth of financial and productive assets, debt-financed expansion) structurally incompatible with material throughput reduction, or can financial accumulation decouple from material throughput through asset-value inflation, financialization, and service-sector expansion?',
    'Empirical tracking of financial accumulation and material throughput decoupling; accounting for debt-service requirements and capital-replacement cycles; analysis of whether service-sector expansion is genuinely material-light or embeds throughput in supply chains; case studies of degrowth-aligned financial systems (cooperatives, gift economies, community currencies) at scale.',
    'If incompatibility is structural (debt requires throughput expansion to service interest; capital replacement drives material cycling), degrowth reading''s claim that capital accumulation is extractive is supported and institutional transformation (move beyond capitalism) is necessary for constraint enforcement. If decoupling is possible, capital-dependent actors have a viable transition pathway (financial-services economy, asset-based wealth without material throughput) and constraint enforcement burden shifts from institutional dissolution toward financial restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_accumulation_vs_degrowth_incompatibility, conceptual, 'Whether capital accumulation is fundamentally incompatible with material throughput reduction or can be restructured to align.').

omega_variable(
    planetary_boundaries_measurement_certainty,
    'Are the planetary boundary thresholds (climate forcing, biodiversity intactness, nutrient cycling, ocean acidification) measured with sufficient precision and are the safe operating spaces scientifically established or remain contested within the Earth science community?',
    'Peer-review consensus on boundary definitions; IPCC/IPBES assessment cycles; meta-analysis of boundary-setting methodologies; comparison of boundary values across research groups; uncertainty ranges on tipping points and feedback thresholds.',
    'If boundaries are well-established with low uncertainty, degrowth reading''s framing of absolute biophysical limits is justified and constraint enforcement priority is high. If boundaries are scientifically contested (wide uncertainty, disagreement on safe thresholds), the constraint''s claim to non-negotiability is weakened; mitigation and adaptation readings gain credibility as alternative interpretations of the same underlying uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(planetary_boundaries_measurement_certainty, empirical, 'Whether planetary boundaries are scientifically established with sufficient precision to justify material throughput reduction as non-negotiable.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does the degrowth reading''s core premise (planetary boundaries primary; material throughput must decrease) logically foreclose both the mitigation-priority reading (rapid decarbonization with growth) and the adaptation-priority reading (accept warming, invest in resilience), or can all three readings coexist within different institutional frameworks?',
    'Logical analysis of axiom compatibility; test whether accepting planetary-boundaries-primary axiom requires rejecting growth-compatible decarbonization or accepting warming inevitability; examine whether institutional configurations could hold multiple readings simultaneously (subsidiarity: North uses degrowth, South uses growth; time-staged: degrowth in late-industrial, adaptation in vulnerable regions).',
    'If degrowth forecloses both siblings, the kernel exhibits a genuine triadic conflict and one reading must prevail institutionally. If readings can coexist (held by different factions or in different contexts), the constraint is a manifestation of institutional contest rather than scientific resolution; uncertainty persists about which reading will dominate policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether the degrowth reading logically forecloses or coexists with mitigation and adaptation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_response_obligation__degrowth_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__degrowth_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_obligation__degrowth_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__degrowth_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t25, climate_response_obligation__degrowth_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__degrowth_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__degrowth_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_response_obligation__degrowth_reading, base_extractiveness, 5, 0.53).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__degrowth_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_obligation__degrowth_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__degrowth_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t25, climate_response_obligation__degrowth_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__degrowth_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__degrowth_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_response_obligation__degrowth_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__degrowth_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_obligation__degrowth_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__degrowth_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t25, climate_response_obligation__degrowth_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__degrowth_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__degrowth_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, planetary_boundaries_enforcement).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, global_north_consumption_norms).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, capital_accumulation_constraint).

% DUAL FORMULATION NOTE:
% The climate-response-obligation kernel gives rise to three competing readings, each with distinct ε, beneficiary/victim structure, and type. Degrowth reading (this constraint) treats material throughput as primary decision variable and positions high-consumption populations and capital-accumulation actors as primary targets. Mitigation-priority reading treats carbon intensity as primary decision variable and assumes growth can decouple from throughput through efficiency and renewable energy. Adaptation-priority reading treats warming magnitude as given and optimizes for resilience investment rather than prevention. The three readings share commitment to climate crisis response but diverge on what is binding constraint. All three should be authored as separate constraint stories linked via affects_constraints to show the kernel contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
