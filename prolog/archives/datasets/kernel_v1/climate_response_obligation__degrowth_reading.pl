% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Climate Response Obligation (Degrowth Reading): Reduce Material Throughput to Stay Within Planetary Boundaries
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading of the climate response obligation is one of three
 *   competing framings of humanity's obligation to address planetary
 *   overshoot. This reading asserts that remaining within planetary
 *   boundaries requires reducing material throughput (not merely
 *   decarbonizing it), which necessarily means reducing consumption in
 *   high-income countries and constraining development pathways in low-income
 *   countries until and unless the North reduces first. The reading makes
 *   capital accumulation itself a target — not just carbon-intensive capital,
 *   but the growth imperative that drives endless extraction. This is
 *   structurally distinct from the mitigation-priority reading (prevent
 *   warming via rapid decarbonization while preserving growth) and the
 *   adaptation-priority reading (accept inevitable warming and build
 *   resilience). The degrowth reading produces the richest perspectival
 *   variation: it appears as a snare from the perspectives of the
 *   climate-constrained poor, incumbent industries, and wealthy consumption
 *   classes; as a rope for the commons; as a tangled rope for the working
 *   class and moderate actors; and as a scaffold for social movements with
 *   exit paths. The false summit perspective (analytical/natural law)
 *   naturalizes the reading as inherent planetary physics, but the structural
 *   data reveals this as ideological naturalization of political choices
 *   about resource allocation.
 *
 * KEY AGENTS:
 *   - Planetary biophysical systems: Primary beneficiary — degrowth reading exists to reduce extraction pressure on carbon/water/nutrient cycles
 *   - Future human generations: Beneficiary (powerless/trapped in time) — protected from severe warming scenarios by North's current throughput reduction
 *   - Non-human species: Beneficiary (structurally without voice) — reduced extraction pressure enables habitat recovery and population stabilization
 *   - Climate-constrained poor (Global South): Primary victim (powerless/trapped) — development pathways foreclosed; trapped in double bind
 *   - Incumbent extraction industries: Victim (organized/mobile but foreclosed) — business models foreclosed; cannot exit via growth-preserving alternatives
 *   - Current wealthy consumption classes (Global North): Victim (powerful/constrained) — required lifestyle reduction; highest burden falls here
 *   - Industrial working class (high-income countries): Mixed victim-beneficiary (moderate/constrained) — loses extraction-industry jobs but gains alternative livelihood models
 *   - Degrowth movements: Organized actors with agency (organized/constrained) — see constraint as enabling structure with sunset; not victims
 *   - Public commons and regenerative systems: Beneficiary with no agency (institutional/arbitrage in formal terms, but structurally the reading's sole purpose)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.58).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Climate Response Obligation (Degrowth Reading): Reduce Material Throughput to Stay Within Planetary Boundaries").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, 'fa4a99bf-2218-459b-ad4e-0817efcaa716').
narrative_ontology:cs_kernel_codification('fa4a99bf-2218-459b-ad4e-0817efcaa716', distributed).
narrative_ontology:cs_authority_grounding('fa4a99bf-2218-459b-ad4e-0817efcaa716', distributed).
narrative_ontology:cs_reading_relation('fa4a99bf-2218-459b-ad4e-0817efcaa716', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('fa4a99bf-2218-459b-ad4e-0817efcaa716', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_axiom('fa4a99bf-2218-459b-ad4e-0817efcaa716', foundational, growth_sustainability_incompatibility).
narrative_ontology:cs_axiom_status(growth_sustainability_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('fa4a99bf-2218-459b-ad4e-0817efcaa716', growth_sustainability_incompatibility, empirically_contingent).
narrative_ontology:cs_axiom('fa4a99bf-2218-459b-ad4e-0817efcaa716', foundational, sufficiency_primary_justice_mechanism).
narrative_ontology:cs_axiom_status(sufficiency_primary_justice_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('fa4a99bf-2218-459b-ad4e-0817efcaa716', sufficiency_primary_justice_mechanism, deontological).
narrative_ontology:cs_reference_frame('fa4a99bf-2218-459b-ad4e-0817efcaa716', planetary_boundary_sufficiency_equilibrium).
narrative_ontology:cs_drift_state('fa4a99bf-2218-459b-ad4e-0817efcaa716', contemporary_carbon_budget_exhaustion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fa4a99bf-2218-459b-ad4e-0817efcaa716', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_biophysical_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_human_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, non_human_species).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, current_high_consumption_populations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, incumbent_extraction_industries).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, growth_dependent_capital_regimes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-CONSTRAINED POOR (SNARE) — Trapped by the double bind: development pathways are carbon-intensive (following the historical route the Global North took), yet planetary boundaries prevent them from accessing those pathways without catastrophic warming. No exit. North's affluence was built on throughput that is now forbidden to South. Pure extraction from the perspective of those whose legitimate development aspirations are sacrificed to keep wealthy nations' consumption patterns viable.
constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDUSTRIAL WORKING CLASS (TANGLED ROPE) — Constrained by employment in carbon-intensive sectors; job losses are real and imminent. But also benefits from the coordination function: degrowth reading offers genuine alternative economic models (shorter working weeks, local production, care economy expansion) that could provide employment and livelihood stability. Extraction is real (sector transition risk) but so is coordination benefit (alternative economic organization). Not helpless, not fully trapped — moderately constrained with some exit paths and some systemic benefits.
constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC COMMONS AND REGENERATIVE SYSTEMS (ROPE) — Sole beneficiary of reduced throughput. The constraint exists precisely to benefit this actor: lower material extraction pressure means soil regeneration, watershed recovery, forest regrowth, fishery restoration, atmospheric stabilization. This perspective experiences zero extraction and pure coordination benefit. The constraint is designed to serve this beneficiary's interests.
constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT EXTRACTION INDUSTRIES (SNARE) — Organized actors (fossil fuel, industrial agriculture, fast fashion, automotive, aviation sectors) face existential threat to their business models. Mobile enough to shift capital to other domains, but the degrowth reading forecloses their preferred exit strategy: decarbonization-while-maintaining-growth. This reading defines away their escape hatch. High suppression of alternatives (must shrink, not shift). Experiencing this as maximum extraction despite organizational power — the reading's core premise forecloses their pathway.
constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: WEALTHY CONSUMPTION CLASSES (TANGLED ROPE) — Primary victims within this reading. Constrained by the requirement to reduce consumption/material throughput (lifestyle reduction, smaller housing, fewer luxury goods, less frequent air travel). Significant extraction. But also benefits from coordination function: the reading promises genuine improvements in non-material wellbeing (community, leisure, health, meaning) and protection against civilizational collapse. Extraction is real (consumption reduction), but coordination benefit is also structurally present (alternative flourishing models). Not experiencing this as pure snare because the reading offers a coherent alternative framing of wellbeing that doesn't depend on accumulation.
constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DEGROWTH MOVEMENTS AND ALTERNATIVE INSTITUTIONS (SCAFFOLD) — Organized agents (cooperatives, transition towns, commons-based resource management, mutual aid networks) see degrowth reading as enabling structure with a sunset clause: temporary reduction in material throughput is the bridge to post-industrial stability. Low effective extraction for this actor because they have agency and see the constraint as having an exit path (post-scarcity equilibrium after transition). Their extraction is mainly transition coordination burden, not structural punishment.
constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational timescale and universal scope, the constraint appears to be a natural law: physics of planetary systems (carbon cycle, nutrient cycles, energy flows) creates an absolute floor on how much material throughput is sustainable. Anything above that floor is literally impossible to sustain — not policy choice but physical reality. This perspective naturalizes the degrowth reading as inherent to the planet itself. However, the structural data (beneficiaries, victims, active enforcement required) contradicts the mountain classification — the engine will flag this as a false summit, revealing that the 'laws of nature' framing naturalizes what is actually a contingent political economy of resource allocation.
constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_obligation__degrowth_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading requires genuine material reduction and consumption sacrifice, primarily from high-income populations. This is not coercive through violent enforcement but through systemic restructuring — alternative pathways are suppressed (growth-dependent models are ruled out). The extractiveness is below the snare threshold (0.66) because the reading offers a coherent alternative framing of wellbeing and economic organization that some agents (movements, working class perspectives) experience as coordinative benefit rather than pure extraction. However, for incumbent industries and wealthy classes, the extraction is substantial and real. Suppression (0.72): High. The reading suppresses growth-dependent alternatives, incumbent business models, and high-consumption lifestyles. Suppression rises over time as the reading's full implications become clearer (measurement shows 0.55→0.72). The suppression is not violent but structural: within the degrowth reading's framework, there is no legitimate alternative pathway — you reduce throughput or you cause ecological collapse. Theater ratio (0.35): Low. This reading is functionally oriented, not performative. It proposes concrete mechanisms (shorter working weeks, local production networks, commons-based resource management, reduced advertising, material sufficiency standards) rather than symbolic compliance. The theater is lower than mitigation-priority reading because degrowth doesn't rely on technological theater ('net zero by 2050') — it requires actual material change. Theater increases slightly toward present (0.42→0.35) because early implementations focus on functional change, not symbolic credentialing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The planetary systems see pure benefit (rope). The climate-constrained poor see pure extraction with no exit (snare). Incumbent industries see foreclosure of their preferred strategy despite organizational power (snare). Wealthy classes see real consumption reduction with offered alternative wellbeing framing (tangled rope). Working class sees mixed extraction (job loss) and coordination benefit (alternative livelihoods, shorter work, care economy expansion) (tangled rope). Degrowth movements see a temporary constraint with a sunset (scaffold). The analytical observer risks naturalizing this as inherent planetary physics (mountain) when the structural data reveals it as political choices about distribution and accumulation. The perspectival gap is widest between the beneficiary (planetary systems) and the victims (incumbent industries, wealthy classes, constrained developing nations), which is exactly what the reading is designed to achieve: forcing beneficiaries into victim-experiencer roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (planetary systems, future generations, non-human species) derive d ≈ 0.0 (full beneficiary) — extraction flows toward them, suppression of alternatives is designed to protect them. Current wealthy consumption classes derive d ≈ 0.8 (mostly target) — they bear the material extraction (consumption reduction) with some coordination offset. Incumbent industries derive d ≈ 0.9 (nearly full target) — business models are foreclosed and capital must be redirected or abandoned. Climate-constrained poor derive d ≈ 0.95 (maximum target) — locked into the double bind with no exit option. Degrowth movements derive d ≈ 0.4 (mixed) — they experience constraint as coordination burden but see exit path and gain agency from the reading. The institutional actors (systems, commons) have no conventional power axis — their directionality is derived from beneficiary status alone, yielding negative effective extraction (they benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: The degrowth reading resolves the mandatrophy by redefining the victim set compared to mitigation-priority. Mitigation-priority sees victims as those who suffer from unchecked warming (everyone); degrowth reading sees victims as those whose consumption must reduce and those whose development is constrained (wealthy nations first, then poor nations unless North reduces). The two readings have incompatible victim assignments because they make different empirical and normative claims about what 'sustainability' requires. The mandatrophy is not solvable within either reading alone — it requires recognizing that both readings are internally coherent but starting from different premises (decoupling is possible vs. decoupling is impossible). The omega on kernel_reading_foreclosure is the critical diagnostic: if empirical research confirms that decarbonization cannot decouple from growth, then degrowth forecloses mitigation-priority and the mandatrophy collapses into a factual question. If decoupling is possible, then both readings remain live and the mandatrophy is definitional rather than factual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_threshold_indeterminacy,
    'What constitutes ''sufficiency'' in material throughput reduction? Is it per-capita, absolute, weighted by inequality?',
    'Multi-model biophysical accounting (carbon budgets, mineral extraction, water use, land use, biodiversity impact) combined with ethical distribution models; empirical comparison of outcomes under different sufficiency thresholds',
    'If per-capita sufficiency: requires Global North reduction of 60-75%; Global South gets carbon budget increase. If absolute sufficiency: requires global decline; locks Global South into perpetual underdevelopment. If weighted-by-inequality: requires North reduction of 80-90% + South increase; most politically explosive framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_threshold_indeterminacy, conceptual, 'Definition of sufficiency threshold — per-capita, absolute, or inequality-weighted').

omega_variable(
    transition_viability_under_extraction,
    'Can genuine economic transition to degrowth models occur while populations are experiencing the extraction (consumption reduction, job loss, income uncertainty)?',
    'Longitudinal data from historical planned economy transitions, social safety net sufficiency analysis, cross-country comparison of transition economies; tracking of mental health, social cohesion, and political stability during transition phases',
    'If transition is viable under extraction: degrowth reading is structurally sound. If transition requires sufficient material cushion to prevent social collapse: degrowth reading''s extraction level forecloses its own success conditions — the constraint becomes self-defeating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_viability_under_extraction, empirical, 'Whether economic transition is feasible under the material extraction pressure the degrowth reading imposes').

omega_variable(
    capital_replacement_logic_under_degrowth,
    'If incumbent extraction industries are victims (forced to shrink), what economic actors replace their capital accumulation role? Does degrowth reading require capital suppression or merely capital redirection?',
    'Political economy analysis of alternative accumulation mechanisms (commons, mutual aid, gift economy, state provision); empirical cases of non-capitalist economies; analysis of whether degrowth is compatible with any accumulation model',
    'If capital redirection is possible: degrowth reading is compatible with mixed economies. If capital suppression is required: degrowth reading forecloses capitalist economies entirely — victim set expands to include all capital-dependent institutions. Shifts the reading from tangled_rope toward pure snare at powerful/organized levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_replacement_logic_under_degrowth, conceptual, 'Whether capital accumulation is replaced or suppressed under degrowth').

omega_variable(
    knowledge_and_care_economy_scalability,
    'Can knowledge work, care work, education, and creative production (non-material intensive sectors) expand enough to absorb workers displaced from extraction industries and maintain income stability?',
    'Labor economics analysis of sectoral transition capacity; empirical data on care economy employment ceilings; comparison to post-industrial economy case studies (Germany, Scandinavia, Japan); wage and stability analysis',
    'If expansion is sufficient: degrowth reading''s tangled rope classification holds. If expansion is limited: permanent unemployment or underemployment for displaced workers — extraction becomes more severe (snare from working class perspective). Affects whether this reading is politically viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_and_care_economy_scalability, empirical, 'Scalability of non-material sectors to replace extraction industry employment').

omega_variable(
    kernel_reading_foreclosure,
    'Does this reading''s core premise (that sufficiency over efficiency is the path to sustainability) logically foreclose the mitigation_priority reading (that rapid decarbonization preserves growth) or merely provide an alternative?',
    'Logical analysis of whether decarbonization-with-growth is physically possible given planetary boundary constraints; empirical comparison of decarbonization rates to growth rates in existing economies; scenario analysis of whether growth can decouple from extraction',
    'If decoupling is impossible: degrowth forecloses mitigation_priority. If decoupling is uncertain but possible: the readings coexist as competing hypotheses. If decoupling is likely: mitigation_priority is not foreclosed by degrowth reading (both can be right in different scenarios).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, empirical, 'Whether degrowth reading logically forecloses the mitigation-priority reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(degrowth_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(degrowth_tr_t5, climate_response_obligation__degrowth_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(degrowth_tr_t10, climate_response_obligation__degrowth_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(degrowth_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(degrowth_be_t5, climate_response_obligation__degrowth_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(degrowth_be_t10, climate_response_obligation__degrowth_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(degrowth_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(degrowth_su_t5, climate_response_obligation__degrowth_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(degrowth_su_t10, climate_response_obligation__degrowth_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, capital_accumulation_growth_imperative).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, global_north_consumption_dependency).

% DUAL FORMULATION NOTE:
% The climate response obligation kernel has three distinct readings, each producing a different constraint with different ε values. Degrowth_reading (this story) has ε=0.58, tangled_rope from analytical perspective. Mitigation_priority reading has ε≈0.40, rope from analytical perspective (assumes decoupling is possible). Adaptation_priority reading has ε≈0.42, tangled_rope from analytical perspective (accepts warming but offers alternative resource allocation). These are not the same constraint viewed from different angles — they are different constraints derived from different reading of the same kernel. The readings form a family linked by network edges; each story declares the sibling constraint IDs in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, institutional, 0.05).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, analytical, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
