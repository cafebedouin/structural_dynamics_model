% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Climate Mitigation via Emissions Reduction and Carbon Markets (maintaining GDP growth)
 *   domain: environmental/economic/political
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate response frames the problem as
 *   requiring emissions reduction to limit warming below 2°C, enabled by
 *   technological innovation (renewables, carbon capture, efficiency) and
 *   market mechanisms (carbon pricing, green finance), while preserving GDP
 *   growth as the organizing principle of economic policy. This reading is
 *   one pole of a three-way contest: it competes against adaptation-priority
 *   (invest in resilience, accept higher warming) and degrowth-transformation
 *   (reject growth as goal, prioritize sufficiency and equity). This story
 *   instantiates the mitigation reading only—it does not average across
 *   readings or describe the contest as internal ambiguity. The reading's
 *   beneficiaries (high-innovation-capacity nations, technology vendors,
 *   incumbent fossil majors negotiating managed transition) and victims
 *   (vulnerable regions, future generations, stranded workers) are
 *   structurally asymmetric. The constraint is claimed as tangled_rope: it
 *   coordinates genuine collective action on a commons problem while
 *   extracting asymmetric costs from those least responsible for the problem
 *   and least able to exit.
 *
 * KEY AGENTS:
 *   - High-innovation-capacity nations (US, EU, Japan, etc.): agenda setters; benefit from technological leadership; defer adaptation costs elsewhere
 *   - Carbon technology vendors: beneficiaries; extract rents from subsidized renewable and carbon-capture markets
 *   - Incumbent fossil majors: payers + beneficiaries; pay transition costs in near term but framework assumes managed transition, not phase-out
 *   - Vulnerable climate regions (small islands, LDCs, agriculture/water-dependent areas): victims; pay in lives and livelihoods while bearing deferred impacts
 *   - Future generations (born post-2050): victims; inherit residual 2°C warming, sea level rise, ecosystem collapse
 *   - Workers in stranded sectors: victims; face job losses and community disruption as transition occurs
 *   - Adaptation-priority excluded actors: would redirect resources to resilience and climate justice; voice present but powerless
 *   - Degrowth advocates (entirely foreclosed): argue growth incompatible with climate solution; not part of official policy space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.54).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Climate Mitigation via Emissions Reduction and Carbon Markets (maintaining GDP growth)").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "environmental/economic/political").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, 'ca231adf-256b-49d5-8a53-10b13cf9bbb0').
narrative_ontology:cs_kernel_codification('ca231adf-256b-49d5-8a53-10b13cf9bbb0', fixed_text).
narrative_ontology:cs_authority_grounding('ca231adf-256b-49d5-8a53-10b13cf9bbb0', extraction).
narrative_ontology:cs_interpretation_layer_present('ca231adf-256b-49d5-8a53-10b13cf9bbb0').
narrative_ontology:cs_reading_relation('ca231adf-256b-49d5-8a53-10b13cf9bbb0', climate_response_action__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('ca231adf-256b-49d5-8a53-10b13cf9bbb0', climate_response_action__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('ca231adf-256b-49d5-8a53-10b13cf9bbb0', foundational, growth_technology_climate_compatibility).
narrative_ontology:cs_axiom_status(growth_technology_climate_compatibility, holdable).
narrative_ontology:cs_axiom_grounding('ca231adf-256b-49d5-8a53-10b13cf9bbb0', growth_technology_climate_compatibility, empirically_contingent).
narrative_ontology:cs_axiom('ca231adf-256b-49d5-8a53-10b13cf9bbb0', foundational, carbon_removal_and_renewable_scaling_feasibility).
narrative_ontology:cs_axiom_status(carbon_removal_and_renewable_scaling_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('ca231adf-256b-49d5-8a53-10b13cf9bbb0', carbon_removal_and_renewable_scaling_feasibility, empirically_contingent).
narrative_ontology:cs_reference_frame('ca231adf-256b-49d5-8a53-10b13cf9bbb0', climate_stabilization_via_technology_and_markets).
narrative_ontology:cs_drift_state('ca231adf-256b-49d5-8a53-10b13cf9bbb0', post_empirical_challenges_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ca231adf-256b-49d5-8a53-10b13cf9bbb0', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, high_innovation_capacity_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_technology_vendors).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, incumbent_fossil_sector_managed_transition).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, vulnerable_climate_regions).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, workers_in_stranded_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, incumbent_fossil_sector_managed_transition).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, technological_substitution_sufficient).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, market_efficiency_carbon_pricing).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, gdp_growth_compatible_with_climate_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advanced economies (US, EU, Japan, Canada, Australia, South Korea) set the international climate agenda through their dominant representation in IPCC science framing, Paris Agreement negotiations, and carbon market design. They benefit disproportionately because their domestic innovation capacity, capital infrastructure, and technological depth position them to develop and export renewable energy, carbon capture, and efficiency solutions. They defer costly adaptation infrastructure investment to vulnerable regions while maintaining growth-oriented economic policies. Their exit option is climate non-participation (reputationally costly but economically profitable for fossil incumbents); instead they choose to shape the commitment's terms to favor their structural advantages.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_innovation_capacity_nations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, high_innovation_capacity_nations, beneficiary).

% Renewable energy manufacturers (solar, wind, battery), carbon capture startups, energy efficiency service providers, and green finance intermediaries extract substantial economic rents from the mitigation-via-innovation pathway. Guaranteed demand from government mandates (renewable portfolio standards, net-zero commitments), subsidies (tax credits, research funding), and mandatory procurement policies create stable profit streams. They profit from constraint operation without bearing its climate or transition costs.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_technology_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Oil, gas, and coal majors negotiate the mitigation pathway's specific terms: they pay near-term carbon pricing costs, transition spending, and stranded-asset write-downs, but the framework's insistence that GDP growth persists means the sector is not phased out—it is decarbonized slowly and managed toward lower emissions while remaining a structural part of the economy. They benefit from market-friendly climate policy that does not demand production phase-out or demand destruction. They retain pricing power and profit margins even as volumes decline, and transition costs are socialized (workers, consumers) rather than absorbed by incumbent firms.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, incumbent_fossil_sector_managed_transition, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, incumbent_fossil_sector_managed_transition, payer).

% Small island nations (Maldives, Tuvalu, Marshall Islands), least-developed countries (Bangladesh, Burkina Faso, Chad), and regions dependent on climate-sensitive sectors (agriculture in sub-Saharan Africa, water resources in Central Asia) face accelerating climate impacts—sea level rise, extreme weather events, resource scarcity—that will occur regardless of global emissions reductions achieved by 2°C target. The mitigation-first framework allocates inadequate adaptation funding to these regions, betting instead on technological solutions that may not arrive in the required timeframe or may fail to scale. They pay in lives, livelihoods, and territory loss while bearing climate impacts the constraint's success was supposed to prevent. They cannot exit without ceasing to exist as territorial entities; they depend on the same international institutions that gatekeep the framework to their disadvantage.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, vulnerable_climate_regions, payer,
    powerless, immediate, trapped, local).

% Humans born after 2050 inherit the residual climate impacts that the mitigation-at-current-ambition pathway does not prevent—a world warmed by 2°C or more with permanent sea level rise (0.5–1 meter), widespread ecosystem collapse, shifted precipitation patterns, and cascading resource scarcity. They bear the consequences of the current generation's choice to maintain growth while making insufficient emissions reductions. They have no voice in today's mitigation framework design and cannot opt out of the world they inherit. Intergenerational extraction occurs because current growth is purchased by accepting higher downstream climate burden.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Coal miners, oil rig workers, refinery employees, power-plant operators, and other fossil fuel industry workers face employment loss and income disruption as the constraint's enforcement transitions away from carbon-intensive production. The mitigation-via-technology framework promises retraining programs, income support, and transition funding, but these are often inadequate and arrive late. The actual costs—permanent loss of high-wage employment, pension erosion, community economic collapse, identity disruption tied to occupational identity—are borne by workers and their communities, not by technology vendors or wealthy nations. Worker mobility is constrained by geographic immobility, credential-lock-in to fossil sectors, and age (older workers have lower reemployment rates).
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, workers_in_stranded_sectors, payer,
    moderate, biographical, constrained, national).

% The IPCC Assessment Reports, peer-reviewed climate science journals, and the broader scientific consensus community provide the empirical foundation for the 2°C warming target and technical feasibility claims underlying the mitigation framework. They observe the constraint from the analytical seat, providing evidence that underpins agenda-setter framing. They also internally debate the adequacy of current mitigation pathways: many climate scientists note that 2°C remains dangerous and that current policies are insufficient; others highlight carbon removal and technological solution uncertainties. Science consensus is used to justify the framework's terms, but scientific uncertainty about technological feasibility and sufficiency is present and growing.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_science_consensus_body, observer,
    analytical, generational, analytical, universal).

% Governments of vulnerable nations (particularly island and least-developed countries), climate justice NGOs, and adaptation-focused development organizations argue that the mitigation-priority framework inadequately funds and prioritizes adaptation for regions facing imminent climate impacts. They are formally present in UNFCCC negotiations and Paris Agreement review cycles, with dedicated Adaptation Committee structures, but they remain excluded from agenda-setting and resource allocation decisions. Mitigation spending vastly outpaces adaptation funding (roughly 10:1 ratio in climate finance); their voices are heard in side events but do not redirect the framework's core priorities. Their constraint is institutional gatekeeping: they can speak but not decide.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, adaptationist_governments_and_ngos, excluded,
    organized, biographical, constrained, national).

% Scholars in ecological economics, degrowth movements, indigenous-led alternatives, and postcapitalist imagining argue that GDP growth is structurally incompatible with climate stabilization and that a livable climate requires moving toward sufficiency, reduced resource throughput, and economic restructuring away from accumulation logic. They are entirely excluded from UNFCCC policy space and mainstream climate governance. The mitigation framework's foundational axiom—that growth and climate response are compatible—structurally forecloses their proposal without debate. They cannot participate in the constraint's design because the constraint exists precisely to defend growth; their exit is identity-locked because disputing growth means leaving the epistemic frame of mainstream economics and policy, which many find intellectually and professionally costly.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, degrowth_and_postcapitalist_advocates, excluded,
    organized, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__mitigation_priority, high_innovation_capacity_nations).
narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns global emissions reduction targets with national policy and market mechanisms (carbon pricing, renewable mandates, technology R&D), solving the collective-action problem of atmospheric carbon as a global commons: no unilateral nation can stabilize climate, so a framework for coordinated reduction is necessary.
% TRANSFER_FUNCTION: Moves adaptation costs and residual climate impacts from high-emitting nations to vulnerable regions; moves near-term transition costs from technology vendors to workers in fossil sectors and communities dependent on cheap energy; moves long-term climate risks from current generations to future generations; transfers wealth from fossil fuel majors to renewable energy and carbon capture companies via subsidy and carbon pricing structures.
% ABSENT_VOICES: Adaptation-priority governments and climate-justice advocates are present in UNFCCC negotiations but excluded from agenda-setting and resource allocation decisions; degrowth and postcapitalist economies arguments are entirely foreclosed by the framework's growth axiom and do not appear in official climate policy discourse; indigenous communities and small-island nations have consultative roles but no veto power over mitigation pathway design.
% DISAPPEARANCE_RATIONALE: If the mitigation-via-technology framework and its carbon markets vanished, fossil fuel use would accelerate absent alternative regulations, renewable deployment would slow, international climate cooperation would collapse into unilateral national policies, and the global economy would face either rapid climate impacts or forced rapid decarbonization through crisis—neither a stable rearrangement but both would be substantially different from today's managed trajectory.
% FOUNDING_PROBLEM: Atmospheric CO2 concentrations are rising due to industrial emissions; unmitigated warming beyond 2°C poses severe risks to ecosystems and human societies; unilateral national action is insufficient because emissions are a global atmospheric problem; coordinated international emissions reduction targets with market mechanisms were designed to align incentives for collective reduction without requiring complete economic transformation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (CO2 accumulation and its physical climate impacts) is corroborated by atmospheric physics, paleoclimate data, and observational records outside any policy reading—the physical fact is not contested. The framings of what response is required and what transformation costs are acceptable ARE contested: IPCC science bodies attest the 2°C framing is scientifically grounded; adaptation-focused researchers and vulnerable nations attest the founding problem is inadequately addressed by mitigation-only pathways; degrowth scholars attest the growth axiom is incompatible with the founding problem's actual solution. The founding problem is live, but its adequate response is not.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 by 2050) because the constraint systematically defers costs onto those least responsible and least able to exit—vulnerable nations bear climate impacts mitigation did not prevent; future generations inherit residual warming; workers lose livelihoods. Suppression is moderate-high (0.54) because the constraint requires active enforcement of carbon pricing, renewable mandates, and technology deployment against both incumbent fossil interests and adaptationist/degrowth alternatives. Theater is moderately high (0.42) because much of enforcement activity defends the 'growth + technology = climate response' narrative against empirical challenges (carbon removal scaling, renewable intermittency, behavioral lock-in) rather than directly mitigating emissions. Accessibility collapse is low (0.48) because alternatives—adaptation, degrowth, fossil fuel acceleration—remain politically and technically available even if excluded from the framework; the constraint does not close off choices, it reshapes their cost. Resistance is high (0.72) because vulnerable nations, workers, and the adaptationist coalition actively push back, and because physical climate impacts increasingly falsify the 'technology will solve it' narrative. The measurement series show extractiveness and theater rising from 2015 to 2035 and plateauing thereafter—the constraint's core structure is not expected to shift even as enforcement costs rise.
 *
 * PERSPECTIVAL GAP:
 *   From the high-innovation-nation seat, the constraint is a genuine coordination solution: it aligns global emissions reduction with national growth, enables technology markets, and preserves the economic model. From the vulnerable-region seat, the same constraint is predatory: it defers their adaptation funding while betting on technologies that may not scale in time, leaving them to absorb residual impacts. From the future-generation seat (computational/analytical), the constraint is a temporal transfer mechanism: current growth is subsidized by accepting higher downstream climate costs. From the degrowth seat (excluded/analytical), the constraint is a category error—growth incompatibility with climate targets is structurally foreclosed by the reading's axioms, not open for debate. The engine should compute these divergences from the stakeholder power, exit, and beneficiary/victim assignments; the claimed type (tangled_rope) is the committer's structural reading, not a prediction of the engine's verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary seats (high-innovation nations, tech vendors) derive low directionality (d ~0.1–0.3) from the structural data: they benefit without bearing proportional costs, they have arbitrage/mobile exit options, their power is high, so effective extraction favors them. The victim seats (vulnerable regions, future generations, stranded workers) derive high directionality (d ~0.7–0.9): they bear concentrated costs, their exit is trapped or identity-locked, their power is low, and the constraint's operation asymmetrically impacts them. The incumbent fossil majors sit at a paradoxical position: they are nominal payers (carbon costs, transition spending) but also beneficiaries of a framework that does not question growth or demand destruction. No directionality override is required; the derivation chain captures the asymmetry through the beneficiary/victim declarations and the stakeholders' power atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (CO2 accumulation, climate risk) is live and real; its adequate response is contested. The mitigation-priority reading assumes the founding problem is solved by reducing emissions TO 2°C and assumes technological solutions (renewable scaling, carbon capture, efficiency) are sufficient to achieve this reduction while preserving growth. If emissions reduction proves slower than assumed and residual warming exceeds 2°C despite all enforcement, the founding problem has not been solved—the mitigation pathway has failed its mandate. This is distinct from mandatrophy per se: mandatrophy occurs when the problem the constraint was built to solve has died but the constraint persists. Here, the problem remains live; the question is whether the chosen response is adequate. Mandatrophy resolution is NOT applicable because the founding problem has not been solved away. However, there IS a temporal-vindication question: if by 2050 technological carbon removal has not scaled as promised, and mitigation efforts have held warming to only 2.1°C instead of the 1.5–2.0 target, the constraint will have failed its mandate while persisting—a zombie state, but justified by 'we tried and climate science was uncertain.' This is a form of mandate collapse under persistent constraint, captured by the theater and resistance measurements rising together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_carbon_removal_feasibility,
    'Can carbon capture and removal technologies scale to the volumes required (gigatons per year) to achieve 2°C target while maintaining GDP growth, or is technological feasibility overstated?',
    'Empirical tracking of carbon removal deployment rates, cost curves, and energy requirements through 2035–2050. Comparison with modeled projections from mitigation scenarios (IPCC AR6, IEA NZE).',
    'If carbon removal fails to scale as assumed, the 2°C target becomes infeasible without deeper emissions reductions (requiring demand destruction and degrowth-like measures) or higher residual warming (requiring massive adaptation spending). Classification would shift from tangled_rope toward snare (extraction without coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_carbon_removal_feasibility, empirical, 'Whether technological carbon removal achieves the scaling required by mitigation scenarios.').

omega_variable(
    growth_compatibility_with_climate_targets,
    'Is continued GDP growth in high-income nations structurally compatible with the emissions reductions required for 2°C, or does reaching the target require absolute reduction in material throughput?',
    'Decoupling analysis: tracking whether global GDP growth and global emissions growth decoupling persists through 2035–2050. Historical data show decoupling in high-income nations but not globally; future decoupling depends on whether renewable scaling and efficiency gains outpace growth in absolute consumption.',
    'If global decoupling fails and emissions remain coupled to growth, the mitigation pathway''s growth axiom becomes false—the constraint would be sustaining growth by deferring climate costs rather than solving the climate problem. This would reclassify the constraint from tangled_rope (hybrid coordination/extraction) toward snare (pure extraction riding on false coordination narrative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_compatibility_with_climate_targets, empirical, 'Whether GDP growth and emissions reduction are genuinely decoupled or the decoupling is illusory.').

omega_variable(
    kernel_reading_relation_actual_versus_theorized,
    'Do the adaptation-priority and degrowth-transformation readings actually foreclose/coexist_with/influence the mitigation reading in practice, or does institutional power and discourse gatekeeping override structural logic?',
    'Qualitative analysis of UNFCCC and national climate policy debates: do nations pursuing adaptation agendas experience the mitigation framework as a logical alternative (coexist) or as a structurally imposed constraint (forecloses/influences)? Can degrowth proposals gain institutional traction or are they categorically excluded?',
    'If institutional power produces foreclosure behavior that the reading_relations axioms say should be coexistence, the distinction between logical and political foreclosure becomes the operative axis. Classification would remain tangled_rope, but with the understanding that extraction persistence depends on gatekeeping and agenda-setting power, not structural logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relation_actual_versus_theorized, conceptual, 'Whether kernel relations are determined by structural logic or institutional power.').

omega_variable(
    intergenerational_extraction_framing,
    'Is the asymmetric distribution of costs across generations (current growth subsidized by future climate impacts) best framed as extraction (future people as victims of a constraint designed to benefit current people), or as a tragic temporal commons problem where no generation can fully solve climate without constraining the next?',
    'Normative framework analysis: does the intergenerational distribution satisfy the schema''s definition of extraction (identifiable beneficiaries/victims, asymmetric costs), or is it a shared global commons problem where the cost distribution is not designed but emergent?',
    'If intergenerational transfer is framing-dependent, the victims array (''future_generations'') is reading-indexed: an adaptation-priority or degrowth reading would assign lower intergenerational extraction. This reading''s ε (0.68) reflects the mitigation reading''s choice to maintain growth; a degrowth reading''s ε would be lower because it dissolves the growth/climate tradeoff by rejecting growth. No reclassification, but recognition that ε is reading-indexed (per DP-001, OQ-26).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_framing, preference, 'Whether intergenerational cost distribution is extraction or tragic commons.').

omega_variable(
    carbon_market_genuine_coordination_versus_rent_capture,
    'Do carbon markets and carbon pricing genuinely solve the atmospheric commons problem, or do they primarily create rent-capture opportunities for financial intermediaries while allowing high-emitters to offset rather than reduce?',
    'Empirical analysis of carbon offset integrity, additionality, and leakage; tracking whether carbon pricing produces emissions reduction or merely externalities shifting (outsourcing emissions to unpriced sectors).',
    'If carbon markets are rent-capture disguised as coordination, the constraint''s ''tangled rope'' classification shifts: the coordination function (aligning emission reduction) becomes theater, and the extraction function (moving wealth to offset vendors and high-emitters buying cheap offsets) becomes primary. This would support reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_market_genuine_coordination_versus_rent_capture, empirical, 'Whether carbon markets coordinate emissions reduction or capture rents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 2015, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2015, climate_response_action__mitigation_priority, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2025, climate_response_action__mitigation_priority, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(clim_tr_t2025, observed).
narrative_ontology:measurement(clim_tr_t2035, climate_response_action__mitigation_priority, theater_ratio, 2035, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2035, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_response_action__mitigation_priority, theater_ratio, 2050, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2015, climate_response_action__mitigation_priority, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2025, climate_response_action__mitigation_priority, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(clim_be_t2025, observed).
narrative_ontology:measurement(clim_be_t2035, climate_response_action__mitigation_priority, base_extractiveness, 2035, 0.68).
narrative_ontology:measurement_basis(clim_be_t2035, projected).
narrative_ontology:measurement(clim_be_t2050, climate_response_action__mitigation_priority, base_extractiveness, 2050, 0.68).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2015, climate_response_action__mitigation_priority, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2025, climate_response_action__mitigation_priority, suppression_requirement, 2025, 0.48).
narrative_ontology:measurement_basis(clim_su_t2025, observed).
narrative_ontology:measurement(clim_su_t2035, climate_response_action__mitigation_priority, suppression_requirement, 2035, 0.54).
narrative_ontology:measurement_basis(clim_su_t2035, projected).
narrative_ontology:measurement(clim_su_t2050, climate_response_action__mitigation_priority, suppression_requirement, 2050, 0.54).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, renewable_energy_scaling_technological_viability).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, carbon_removal_deployment_feasibility).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, global_fossil_fuel_subsidy_structure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_action kernel. The sibling constraints climate_response_action__adaptation_priority and climate_response_action__degrowth_transformation instantiate competing framings of climate response with different victim sets, different beneficiary structures, and different ε values. All three readings share the same referent (rising atmospheric CO2 and associated climate risks) but construct different claims about adequate response. The mitigation_priority reading assumes technological solutions and growth compatibility; siblings reject these assumptions. Network edges capture both the logical relations (coexists_with, forecloses) and the institutional influence (this reading's dominance in UNFCCC narrows policy space for alternatives). Downstream constraints (renewable scaling, carbon removal feasibility, fossil subsidy structure) inherit the constraint's assumptions about technological sufficiency and growth preservation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
