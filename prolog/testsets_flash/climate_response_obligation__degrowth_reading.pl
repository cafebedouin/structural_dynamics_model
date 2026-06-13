% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Obligation for Planetary Boundaries
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of the broader climate
 *   response obligation. It asserts that staying within planetary boundaries
 *   requires a fundamental reduction in material throughput, prioritizing
 *   sufficiency over efficiency. This implies a significant restructuring of
 *   global economies, particularly in the Global North, and challenges
 *   conventional notions of development. The constraint is framed as a Snare
 *   because it imposes severe costs and lifestyle changes on powerful
 *   incumbent interests and high-consuming populations, with limited exit
 *   options, while claiming to benefit diffuse entities like planetary
 *   ecosystems and future generations.
 *
 * KEY AGENTS:
 *   - Planetary Ecosystems: Primary beneficiary (analytical/trapped) — direct recipient of reduced extraction pressure.
 *   - Future Generations: Primary beneficiary (analytical/trapped) — inherit a more stable planet.
 *   - Degrowth Advocates: Agenda setter (organized/constrained) — actively promotes and seeks to implement the constraint.
 *   - Global North Consumers: Primary payer (moderate/identity_locked) — required to reduce consumption, facing lifestyle changes.
 *   - Fossil Capital Industries: Primary payer (institutional/constrained) — faces existential threat to business model.
 *   - Developing Nations Seeking Growth: Payer (organized/constrained) — constrained in their development pathways.
 *   - Neoclassical Economists: Excluded (institutional/identity_locked) — their growth-centric models are incompatible with this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, snare).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Obligation for Planetary Boundaries").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '8d9fb035-2979-417c-9374-e58e8ae7a90a').
narrative_ontology:cs_kernel_codification('8d9fb035-2979-417c-9374-e58e8ae7a90a', distributed).
narrative_ontology:cs_authority_grounding('8d9fb035-2979-417c-9374-e58e8ae7a90a', diffuse_epistemic).
narrative_ontology:cs_reading_relation('8d9fb035-2979-417c-9374-e58e8ae7a90a', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('8d9fb035-2979-417c-9374-e58e8ae7a90a', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('8d9fb035-2979-417c-9374-e58e8ae7a90a', foundational, material_throughput_must_decrease).
narrative_ontology:cs_axiom_status(material_throughput_must_decrease, holdable).
narrative_ontology:cs_axiom_grounding('8d9fb035-2979-417c-9374-e58e8ae7a90a', material_throughput_must_decrease, empirically_contingent).
narrative_ontology:cs_axiom('8d9fb035-2979-417c-9374-e58e8ae7a90a', foundational, sufficiency_over_efficiency).
narrative_ontology:cs_axiom_status(sufficiency_over_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('8d9fb035-2979-417c-9374-e58e8ae7a90a', sufficiency_over_efficiency, deontological).
narrative_ontology:cs_reference_frame('8d9fb035-2979-417c-9374-e58e8ae7a90a', pre_industrial_ecological_balance).
narrative_ontology:cs_drift_state('8d9fb035-2979-417c-9374-e58e8ae7a90a', contemporary_overshoot_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8d9fb035-2979-417c-9374-e58e8ae7a90a', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_ecosystems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_capital_industries).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, developing_nations_seeking_growth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefits from reduced material throughput and extraction, leading to lower pollution, biodiversity loss, and climate disruption. Its 'well-being' is the ultimate goal of the constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_ecosystems, beneficiary,
    analytical, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_ecosystems).

% Inherit a more stable and habitable planet with resources preserved, avoiding the severe impacts of ecological overshoot. Their interests are represented by advocates in the present.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    analytical, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, future_generations).

% Propose and promote policies aimed at reducing material and energy consumption, advocating for sufficiency, redistribution, and ecological justice. They seek to implement the constraint through political and social movements.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, generational, constrained, global).

% Are required to significantly reduce their consumption patterns and material footprint, potentially sacrificing accustomed lifestyles and economic growth. Their identity is often tied to consumerism and affluence.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumers, payer,
    moderate, biographical, identity_locked, global).

% Face existential threats to their business models due to mandates for reduced material throughput and a shift away from extractive industries. They resist the constraint through lobbying and political influence.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_capital_industries, payer,
    institutional, immediate, constrained, global).

% Are asked to forgo or significantly limit their economic development pathways that rely on increased material consumption, even if their historical contribution to ecological overshoot is minimal. This is often framed as 'climate colonialism'.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, developing_nations_seeking_growth, payer,
    organized, generational, constrained, global).

% Their models and policy recommendations are often based on continuous economic growth and efficiency gains, which are fundamentally challenged by the degrowth imperative. They are excluded from the core policy-making process of this reading.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, neoclassical_economists, excluded,
    institutional, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human economic activity to operate within the biophysical limits of the planet, ensuring long-term ecological stability and equitable resource distribution for all species and future generations.
% TRANSFER_FUNCTION: Transfers material and energy resources from current high-consuming societies (primarily Global North) to planetary sinks and future generations, by reducing throughput and reallocating existing wealth.
% ABSENT_VOICES: The voices of future generations and non-human species are structurally absent, represented only by advocates. Neoclassical economists, whose models are incompatible with degrowth, are also excluded from the core policy discourse of this reading.
% DISAPPEARANCE_RATIONALE: If the degrowth obligation vanished, the world would revert to a growth-at-all-costs paradigm, accelerating ecological overshoot and climate breakdown, leading to severe and irreversible planetary destabilization. The current economic system is fundamentally incompatible with planetary boundaries without this constraint.
% FOUNDING_PROBLEM: Humanity's economic activity has exceeded planetary boundaries, leading to climate change, biodiversity loss, resource depletion, and social inequality, threatening the long-term habitability of Earth.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on planetary boundaries (e.g., Stockholm Resilience Centre, IPCC reports) and ecological economics provides strong corroboration from outside the immediate degrowth advocacy groups. Indigenous knowledge systems also corroborate the need for living within ecological limits.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.85) is high because the constraint demands a radical reorientation of economic systems, imposing significant costs on current high-consuming populations and industries. Suppression (0.70) is also high, as the implementation of degrowth policies would require overcoming immense political and economic resistance, potentially through strong regulatory measures. Theater ratio (0.10) is low, as the degrowth movement is generally direct and explicit about its goals, with little performative cover for other agendas. Resistance (0.90) is very high, reflecting the profound challenge this reading poses to established economic and political orders. Accessibility collapse (0.60) is moderate, as while the current system offers 'alternatives' (e.g., green growth), this reading argues they are insufficient and ultimately collapse into the same extractive paradigm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of planetary ecosystems and future generations (represented by degrowth advocates), this is a necessary and just rebalancing. From the perspective of Global North consumers and fossil capital industries, it is a severe imposition, threatening their way of life and economic viability. Developing nations also experience a perspectival gap, as they are asked to limit growth that historically benefited the North, leading to accusations of 'climate colonialism'. The engine's classification as a Snare reflects the high extraction and suppression experienced by the payers, despite the claimed benefits for diffuse entities.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary ecosystems and future generations are full beneficiaries (d=0.0) as the constraint directly reduces extraction pressure on them. Degrowth advocates are agenda setters, aligning with beneficiaries. Global North consumers and fossil capital industries are full targets (d=1.0) due to the direct and severe costs imposed. Developing nations seeking growth are also targets, as their development is constrained. Neoclassical economists are excluded, their models rendered irrelevant by the constraint's premises.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (staying within planetary boundaries) is increasingly urgent and 'live'. However, the contestation around its 'founding problem status' (live vs. contested by other readings) highlights the political nature of defining the problem itself. The Snare classification prevents mislabeling this as a 'Rope' or 'Scaffold' by acknowledging the severe, enforced extraction from identifiable victims, even if the ultimate goal is planetary well-being. It highlights that even well-intentioned constraints can operate as Snares if they impose costs coercively without sufficient alternatives or consent from those who bear the burden.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_necessity_empirical,
    'Is a global reduction in material throughput (degrowth) empirically necessary to stay within planetary boundaries, or can technological innovation and efficiency gains (green growth) achieve the same goal?',
    'Long-term empirical data on decoupling economic growth from resource use and environmental impact, and modeling of future scenarios under different policy regimes.',
    'If degrowth is empirically necessary, the constraint''s high extractiveness and suppression are justified as unavoidable costs of planetary survival. If green growth is sufficient, the constraint''s extractive nature would be reclassified as an unnecessary Snare, as less coercive alternatives exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(degrowth_necessity_empirical, empirical, 'Empirical necessity of degrowth vs. sufficiency of green growth.').

omega_variable(
    intergenerational_justice_framing,
    'Is the degrowth obligation primarily an issue of intergenerational justice (preserving resources for future generations) or intragenerational justice (redistributing resources among current generations)?',
    'Analysis of policy design: whether policies prioritize absolute reduction in consumption or equitable redistribution of existing consumption capacity.',
    'If primarily intergenerational, the constraint''s beneficiaries are diffuse and future-oriented. If primarily intragenerational, the constraint''s beneficiaries become more concrete (e.g., Global South populations receiving redistributed resources), potentially shifting its classification towards a Tangled Rope if the coordination function for redistribution is clear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_justice_framing, conceptual, 'Framing of degrowth as intergenerational vs. intragenerational justice.').

omega_variable(
    global_south_development_path,
    'Can developing nations achieve genuine well-being and equity without increasing material throughput, or does the degrowth imperative unfairly constrain their development?',
    'Case studies of ''post-growth'' development models in the Global South, and participatory research with affected communities on their definitions of well-being and development.',
    'If alternative development paths are viable, the constraint''s impact on developing nations is less extractive. If not, the constraint''s ''victim'' status for these nations is amplified, reinforcing its Snare classification and raising ethical concerns about ''climate colonialism''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_development_path, empirical, 'Viability of post-growth development for the Global South.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 1970, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1970, climate_response_obligation__degrowth_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(clim_tr_t1990, climate_response_obligation__degrowth_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(clim_tr_t2010, climate_response_obligation__degrowth_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__degrowth_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__degrowth_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t1970, climate_response_obligation__degrowth_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(clim_be_t1990, climate_response_obligation__degrowth_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(clim_be_t2010, climate_response_obligation__degrowth_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__degrowth_reading, base_extractiveness, 2030, 0.85).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__degrowth_reading, base_extractiveness, 2050, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1970, climate_response_obligation__degrowth_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(clim_su_t1990, climate_response_obligation__degrowth_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(clim_su_t2010, climate_response_obligation__degrowth_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__degrowth_reading, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__degrowth_reading, suppression_requirement, 2050, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is the 'degrowth_reading' of the 'climate_response_obligation' kernel, emphasizing reduced material throughput. It is distinct from 'mitigation_priority' (rapid decarbonization within growth) and 'adaptation_priority' (resilience to warming).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
