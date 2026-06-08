% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive_flat_control, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_directive_flat_control
 *   human_readable: Aneyoshi Stone Directive: Tsunami Risk Coordination and Development Constraint
 *   domain: disaster_anthropology/land_use_governance/institutional_memory
 *
 * SUMMARY:
 *   In 1933, the Sanriku region of Japan experienced a catastrophic tsunami
 *   triggered by an offshore earthquake. The disaster prompted the placement
 *   of a stone marker (or series of markers) inscribed with the directive:
 *   'Do not build your homes below this point.' The marker encoded in
 *   durable, non-writeable physical form the elevation reached by the 1933
 *   tsunami wave, preserving institutional knowledge across generations. Over
 *   the subsequent 90+ years, this stone marker has functioned as a land-use
 *   constraint, stabilizing a prohibition against residential development
 *   below the marked elevation. The constraint operates through multiple
 *   mechanisms: cultural transmission (the marker's meaning is taught and
 *   respected within the community), administrative enforcement (land
 *   registries record the restriction, zoning boards enforce it), legal
 *   precedent (property titles inherit the restriction), and environmental
 *   logic (the marker's location encodes empirical knowledge about past
 *   sea-level transgressions). The constraint exhibits properties of all six
 *   DR types depending on the observer's structural position, making it an
 *   exemplar of how disaster anthropology, institutional memory, and
 *   governance create layered constraints that are simultaneously natural
 *   facts, cultural artifacts, legal instruments, and extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Surviving residents and their descendants (powerless/constrained): Primary beneficiaries — protected from tsunami risk by the directive; also primary victims of development constraints if they later need to migrate or expand settlement
 *   - Land developers (powerful/arbitrage): Secondary victims — constrained from maximizing land use by the restriction; also benefit from stabilized property titles and reduced litigation risk
 *   - Landless poor and low-income families (powerless/trapped): Indirect victims — excluded from the lowest-cost housing by the constraint; face forced displacement to distant, more expensive sites
 *   - Municipal government / land registry (institutional/arbitrage): Secondary beneficiaries — benefit from reduced disaster liability, stabilized tax base, institutional legitimacy through cultural custodianship
 *   - Climate scientists and adaptation planners (institutional/constrained): Analytical observers — see the constraint as transitional scaffolding that will be superseded by digital systems
 *   - Anthropologists and cultural historians (analytical/analytical): Observers of the constraint as an institutional memory technology and a form of long-duration knowledge preservation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive_flat_control, 0.15).
domain_priors:suppression_score(aneyoshi_stone_directive_flat_control, 0.35).
domain_priors:theater_ratio(aneyoshi_stone_directive_flat_control, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, extractiveness, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive_flat_control, tangled_rope).
narrative_ontology:human_readable(aneyoshi_stone_directive_flat_control, "Aneyoshi Stone Directive: Tsunami Risk Coordination and Development Constraint").
narrative_ontology:topic_domain(aneyoshi_stone_directive_flat_control, "disaster_anthropology/land_use_governance/institutional_memory").

domain_priors:requires_active_enforcement(aneyoshi_stone_directive_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(aneyoshi_stone_directive_flat_control, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, surviving_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, future_generations_protected_from_tsunami).
narrative_ontology:constraint_victim(aneyoshi_stone_directive_flat_control, land_developers).
narrative_ontology:constraint_victim(aneyoshi_stone_directive_flat_control, low_income_families_seeking_affordable_housing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, land_developers).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, municipal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents whose lives were saved by chance location above the 1933 tsunami run-up. They and their descendants benefit from the stone directive's encoding of disaster knowledge, which protects them from settling in high-risk zones. They are constrained by geography (they must live somewhere in the region) but the constraint aligns with their survival interest.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, surviving_residents, beneficiary,
    powerless, generational, constrained, local).

% Descendants of the region who inherit knowledge of tsunami risk through the stone marker. They benefit from reduced tsunami mortality, though the marker's meaning becomes increasingly symbolic as lived memory fades.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, future_generations_protected_from_tsunami, beneficiary,
    powerless, generational, mobile, local).

% Developers who want to maximize land use and profit by building on lower-elevation, cheaper land. The constraint forecloses their access to the highest-density, lowest-cost development sites. They benefit from stabilized property titles and reduced litigation risk from the clear boundary, but experience this benefit as insufficient compensation for the development constraint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, land_developers, payer,
    powerful, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive_flat_control, land_developers, beneficiary).

% Families unable to afford housing in safer, higher-elevation areas. The constraint forecloses their access to the cheapest land and forces them to pay more for housing further from historical settlement zones, or to move out of the region entirely. They experience no benefit from tsunami protection (it was not in living memory) and bear full cost of restricted supply.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, low_income_families_seeking_affordable_housing, payer,
    powerless, biographical, trapped, local).

% Local government that maintains the constraint through zoning law, property registration, and enforcement. The municipality benefits from reduced disaster liability, stabilized tax base, and institutional legitimacy as custodian of cultural memory. It also benefits from clear enforcement rules that reduce disputes.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, municipal_government, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive_flat_control, municipal_government, beneficiary).

% Administrative apparatus that records the constraint in property titles and enforces it through title restrictions. The registry maintains the constraint through formal legal mechanisms that persist even after cultural memory fades. Registry staff are constrained to enforce whatever rules are encoded in law.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, land_registry_bureaucracy, agenda_setter,
    institutional, immediate, constrained, regional).

% Elders, teachers, and historians who transmit knowledge of the 1933 disaster and the stone marker's meaning to younger generations. They are constrained by the limit of lived memory (few survivors remain) and the challenge of maintaining cultural transmission as modernization reduces engagement with traditional knowledge. Their role is essential to the constraint's theater: they maintain the performative elements that give the marker symbolic force.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, cultural_memory_keepers, agenda_setter,
    moderate, generational, constrained, local).

% Researchers and policymakers who see the stone directive as a case study in long-duration institutional memory and disaster adaptation. They view the constraint as transitional scaffolding that will be superseded by digital hazard mapping and climate-adjusted risk assessment. They are observers rather than participants in the constraint's operation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, climate_adaptation_planners, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint solves the genuine collective-action problem of preserving knowledge of tsunami run-up elevation across generational spans when oral tradition and institutional memory are fragile. The physical marker provides durable, non-writeable encoding of the disaster boundary that survives bureaucratic turnover, technological change, and the fading of lived memory. Without this encoding, each generation must rediscover the risk through catastrophe.
% TRANSFER_FUNCTION: The constraint transfers opportunity costs (foregone low-cost development) from present developers to future residents by restricting residential settlement in high-risk zones. It also transfers safety benefits (reduced tsunami mortality) from the state (which would bear post-disaster relief costs) to protected residents. The beneficiary of the transfer is the long-duration community; the payer is the present generation of excluded developers and poor families.
% ABSENT_VOICES: Voices excluded from the constraint's formulation: (1) the landless poor of the 1930s who were never asked whether they accepted the restriction, (2) potential future residents who will inherit the constraint without having participated in its original decision, (3) non-human stakeholders whose habitat is affected by the constraint (coastal ecosystems, fisheries). The constraint was authored by survivors and elders without consultation of those who would bear its costs.
% DISAPPEARANCE_RATIONALE: If the stone directive disappeared, the institutional arrangement would bifurcate: wealthy actors could develop below the old marker elevation (if legal restrictions could be modified), but poor families would have nowhere cheaper to go — they would be displaced out of the region. Tsunami risk would increase immediately in low-elevation zones. Within 2-3 generations, if institutional memory also degraded, settlement would creep back down slope toward the old marker line, recreating the pre-1933 pattern until the next tsunami. The disappearance would not return the world to 'unchanged' — it would trigger rearrangement of the residential geography along lines of wealth and risk tolerance.
% FOUNDING_PROBLEM: The founding problem was the 1933 Sanriku tsunami that killed approximately 3,000 people in the region, many of whom lived in low-elevation settlements. The immediate survivors faced the cognitive and institutional challenge of encoding the disaster boundary in a form that would persist after their deaths and across technology change. Oral tradition alone was unreliable — subsequent generations lost connection to the disaster narrative. A physical marker provided durable encoding.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from: (1) Japanese seismic and paleotsunami records confirming that the 1933 tsunami was a rare event in the historical record, (2) demographic records showing the death toll and settlement patterns, (3) physical evidence of the marker's installation in the 1930s, (4) archaeological surveys of tsunami deposits. However, the founding problem's 'deadness' is ambiguous — while the 1933 disaster cannot recur, tsunami risk of similar magnitude remains live (another Sanriku-scale event is geologically possible), and the institutional problem of intergenerational knowledge transfer remains live. The problem is 'dead' only in the sense that the specific historical disaster is in the past; the generic problem (how to preserve disaster knowledge) is not dead.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTECTED COMMUNITY (ROPE) — Powerless agents at generational horizon perceive the stone directive as pure coordination: it solves the genuine collective-action problem of remembering where past tsunamis reached. The directive communicates ancestral knowledge across generations when institutional memory has atrophied. Exit is constrained by locality (one must live somewhere) but the constraint aligns with their survival interest. No extraction is experienced — the beneficiary and the protected are the same group.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: LANDLESS POOR (SNARE) — Powerless agents at biographical horizon perceive the constraint as pure extraction. They are trapped by economic necessity in a housing market where below-stone land was historically the only affordable option. The constraint forecloses the cheapest housing, forcing them toward distant, expensive alternative sites or informal settlement. They experience no coordination benefit — they are excluded from the original disaster knowledge — and bear full cost of constrained housing supply. Suppression is high: economic dependency and geographic immobility lock them in place.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: LAND DEVELOPER (TANGLED ROPE) — Powerful agents with arbitrage options experience the constraint as genuine coordination (it stabilizes property values, establishes clear title, reduces litigation risk) coupled with asymmetric extraction (it forecloses the highest-density development on cheap low-lying land, reducing profit margins). They can arbitrage to alternative development sites but face legitimate carrying costs. The enforcement mechanism is visible and active (zoning boards, land registry, legal penalties) — developers experience it as genuine constraint, not theater.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MUNICIPAL GOVERNANCE (ROPE) — Institutional actors at generational horizon perceive the stone directive as pure coordination: it encodes ancestral disaster knowledge in a durable, non-writeable form that survives bureaucratic turnover and technological disruption. The municipality benefits from reduced disaster liability, stabilized property taxes on protected land, and institutional legitimacy through custodianship of cultural memory. Exit options are high (modify zoning) but costly politically. No extraction is experienced by the municipality — it is the beneficiary of institutional continuity.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At civilizational/universal horizon, the constraint appears as an immutable natural law: tsunami return periods are geological facts, and the stone marker's location encodes empirical knowledge about past sea-level transgressions. From this perspective, the constraint emerges naturally from the physical world — no party creates it, all parties benefit from respecting it, and no enforcement is needed because the stone simply marks where the water goes. However, the structural data contradicts this classification: the constraint has active beneficiaries (municipality, survivors), active victims (developers, the poor), and requires enforcement. The engine will compute this as a false summit, revealing that the 'natural law' framing naturalizes what is actually a contingent institutional arrangement built on top of the physical fact.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CLIMATE ADAPTATION COALITION (SCAFFOLD) — Organized institutional actors (UNESCO, UNISDR, climate scientists) perceive the stone directive as a transitional coordination mechanism with a sunset. The directive is valuable as an example of long-duration institutional memory but will be superseded by digital risk mapping, real-time seismic networks, and climate-adjusted hazard modeling. The directive is effective scaffolding for communities lacking institutional memory infrastructure, but the sunset logic is clear: as digital systems mature globally, the need for stone markers will decrease. The constraint has beneficiaries and victims, but the classification is scaffold because the endpoint is transparent — institutional memory will migrate from durable objects to software systems.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(aneyoshi_stone_directive_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.15, increasing to 0.18 by period 60, then declining): The constraint is low-extraction overall because the coordinated benefit (disaster protection) is substantial and broadly shared. However, extractiveness increases slightly over the first 60 years as the constraint ages and the memory of the original 1933 disaster fades — the coordination function (remembering where tsunamis reached) becomes less obvious and enforcement becomes more bureaucratic. The slight decline after year 60 reflects a stabilization of institutional norms: younger generations accept the constraint as baseline rather than fighting it. Base extractiveness of 0.15 reflects that the core extraction mechanism (excluding the poor from cheap housing) is real but not the primary function of the constraint. SUPPRESSION (0.35, declining from 0.50 to 0.30): Suppression is moderate-high at the origin because the constraint operates through cultural transmission and compliance with ancestral authority — enforcement is normalized within the community and does not require heavy police mechanisms. Suppression declines over time as zoning becomes formal law and institutional infrastructure replaces cultural enforcement. By period 80, suppression is low because the constraint is embedded in property law and does not require active suppression — it is simply a feature of the landscape. THEATER RATIO (0.28, rising from 0.15 to 0.42): Low-to-moderate theater at the origin because the constraint's functional meaning is still legible (survivors and their children understand the disaster connection). Theater rises over time as the memory of 1933 fades and the marker becomes increasingly symbolic — its meaning must be transmitted through cultural education rather than lived memory. By period 80, the theater ratio reflects that the stone marker is increasingly maintained as a cultural artifact and memorial rather than as a direct disaster warning. CLAIMED TYPE: TANGLED ROPE. The constraint exhibits genuine coordination (it solves the problem of preserving disaster knowledge across generations and stabilizing property titles) coupled with asymmetric extraction (it forecloses low-cost development and creates geographic exclusion for the poor). The enforcement mechanism is active and visible (zoning, legal titles, cultural transmission) — it is tangled rope, not rope alone, and not snare (beneficiaries exist and the coordination is genuine).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion across power levels. The powerless agent at generational horizon sees pure rope (the constraint solves their collective survival problem and incurs no extraction cost). The powerless agent at biographical horizon sees pure snare (the constraint forecloses cheap housing and offers no survival benefit from their perspective — they were not in the 1933 tsunami). The powerful agent sees tangled rope (genuine coordination plus asymmetric extraction). The municipality sees rope (institutional stability and legitimacy). The analytical observer at the civilizational horizon risks seeing a mountain (natural law) but the structural data contradicts this: active beneficiaries, active victims, and enforcement mechanisms reveal the constraint as contingent and institutional. The perspectival gap is driven by time horizon (the disaster is visible at generational scales but invisible at biographical scales) and by exit options (wealthy agents can arbitrage to alternative development; poor agents cannot arbitrage to housing). This gap reveals that the same constraint solves different problems for different groups and is experienced as beneficial by survivors and extractive by the excluded.
 *
 * DIRECTIONALITY LOGIC:
 *   The directional values (d = beneficiary→0.0, target→1.0) are derived from beneficiary/victim declarations and power/exit combinations. Surviving residents and their descendants are beneficiaries at generational horizon (d ≈ 0.1); they experience effective extraction as negative or zero (the constraint subsidizes their safety). Land developers are mixed: they are beneficiaries of title stability (d ≈ 0.4 toward equilibrium) but targets of development restriction (d ≈ 0.6 at regional scope). The landless poor are targets (d ≈ 0.85) with trapped exit options — they experience maximum effective extraction despite the constraint's low base ε, because they cannot arbitrage to alternatives. The municipality is a beneficiary (d ≈ 0.2) with arbitrage options (they could modify zoning) but choose not to. The directionality derivation shows how the same constraint produces opposite d values for agents at different power levels and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC SIGNAL: The constraint resolves mandatrophy by showing that the mandate (protect from tsunami) is still live and functional at civilizational and generational horizons, but the constraint's design assumes a specific institutional context (community-based enforcement, memory transmission through cultural channels) that may not persist at biographical horizons when the disaster is no longer a lived memory. The mandatrophy is not resolved — it is DEFERRED. If institutional memory infrastructure (zoning, land registry) persists, the constraint persists even after cultural memory fades. If institutional infrastructure degrades (failed states, war, regulatory collapse), the constraint reverts to piton (maintained as symbol) or disappears entirely. The current classification (tangled rope) is stable only under the assumption that both cultural and institutional enforcement mechanisms persist. The three omegas addressing ritual vs functional enforcement, institutional memory degradation, and climate-driven boundary revision all point to the same underlying fragility: the constraint's mandate is live, but its institutional substrate is not guaranteed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_contingent_institution,
    'Is the constraint a discovery of natural physical law (tsunami run-up elevation is a geological fact) or a contingent institutional arrangement built on top of that fact?',
    'Examine whether the constraint''s persistence depends on institutional maintenance. If the stone marker were destroyed, would the constraint persist? If institutional maintenance ceased (zoning board dissolved, land registry erased), does the constraint persist? Compare tsunami zones where oral tradition survives (rope) vs zones where markers degraded and institutional memory was lost (snare reappears).',
    'If natural law: classification reverts to mountain from all perspectives; no beneficiaries, no victims. If contingent: current tangled_rope classification holds; false summit detector reclassifies the mountain perspective. This omega determines whether the constraint is defensible as trans-institutional vs subject to institutional failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_institution, conceptual, 'Whether the constraint is a natural law or contingent institution').

omega_variable(
    extraction_mechanism_legitimacy,
    'Is the constraint''s asymmetric impact on developers and the poor a necessary cost of disaster protection (legitimate coordination overhead) or a mechanism of class-based exclusion?',
    'Historical comparison: examine whether the directive constrains ALL development equally or whether wealthy actors find loopholes (elevated platforms, engineered barriers, insurance arbitrage). If wealthy actors can develop below the stone marker through technical or legal workarounds, the constraint is selectively enforced and the extraction is revealed. If constraint holds uniformly, extraction is a coordination cost, not a class mechanism.',
    'If selective enforcement: classification shifts toward snare for powerless agents; false summit detection fails on the mountain perspective. If uniform enforcement: classification holds as tangled rope; the extraction is justified as necessary overhead for collective protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_legitimacy, empirical, 'Whether constraint enforcement is equal or selective by wealth').

omega_variable(
    institutional_memory_degradation,
    'What is the constraint''s median institutional memory span? When oral tradition and stone markers are the only substrate for storing disaster knowledge, how many generations survive before the constraint is forgotten?',
    'Archaeological and historical record: survey Japanese tsunami zones for markers that were abandoned, rebuilt, or reinterpreted. Measure the mean interval between marker installation and constraint violation (development below marker). Compare to digital-era zones with continuous institutional memory (hazard databases, legally binding zoning).',
    'If median span < 150 years: the constraint is a scaffold with real sunset logic — digital systems will extend institutional memory indefinitely and the stone marker becomes redundant. If median span > 300 years: the constraint approximates natural law — institutional substrate is stable across centuries. This affects the scaffold classification''s credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_memory_degradation, empirical, 'Median institutional memory span for stone-marker-based land-use constraints').

omega_variable(
    alternative_housing_supply_substitutability,
    'Are housing sites above the stone marker truly alternative (they can substitute for below-marker land) or is below-marker land uniquely scarce in ways that create permanent exclusion?',
    'Housing market analysis: compare land prices above and below the marker; measure commute distance, employment accessibility, and agricultural utility. If above-marker land is equally accessible and affordable, the constraint is a binding but substitutable coordination cost. If above-marker land is systematically distant, expensive, or low-utility, the constraint creates permanent geographic exclusion for the poor.',
    'If substitutable: the snare classification for the poor is overstated — they experience tangled rope (coordination + moderate extraction). If uniquely scarce: the snare classification is confirmed, and the constraint is revealed as a two-tier residential system (protected + unprotected) that correlates with wealth. This affects directionality for the ''landless poor'' stakeholder.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_housing_supply_substitutability, empirical, 'Substitutability of housing sites above vs below the stone marker').

omega_variable(
    ritual_vs_functional_enforcement,
    'To what extent does enforcement of the constraint depend on internalized respect for ancestral knowledge (ritual/cultural maintenance) vs external legal mechanisms (zoning law, title restrictions)?',
    'Ethnographic comparison: examine communities where the stone marker is actively invoked in land-use disputes (visible institutional investment) vs communities where the marker persists as a tourist artifact while zoning law does the actual enforcement (theater). Measure the ratio of cultural-transmission effort to legal-system enforcement effort.',
    'If primarily ritual: the constraint depends on cultural memory maintenance; theater_ratio may be underestimated. If primarily legal: the constraint is institutionalized in law and survives cultural drift. This affects the piton risk — constraints that depend solely on cultural memory may degrade toward piton as younger generations view the marker as folklore rather than binding directive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_vs_functional_enforcement, empirical, 'Ritual vs functional enforcement of the constraint').

omega_variable(
    climate_change_boundary_revision,
    'As sea levels rise and tsunami return periods shift under climate change, does the stone marker''s fixed location become obsolete? Should the boundary move?',
    'Climate modeling and paleotsunami analysis: estimate how tsunami run-up elevation will change by 2100 and 2200 under different climate scenarios. Compare to the marker''s current location. If the optimal safe boundary moves higher or lower, the constraint faces a revision problem: institutional mechanisms for updating the marker location.',
    'If boundary becomes unsafe: the constraint loses its coordination function and becomes pure restriction (snare escalation). If boundary becomes too conservative (unnecessarily forecloses developable land): the constraint becomes piton (maintained as ritual despite diminished function). This omega affects the long-term viability of the constraint''s claimed type and the credibility of the scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_change_boundary_revision, empirical, 'Climate-driven obsolescence risk for the stone marker''s fixed boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive_flat_control, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_tr_t0, aneyoshi_stone_directive_flat_control, theater_ratio, 0, 0.15).
narrative_ontology:measurement(aneyoshi_tr_t20, aneyoshi_stone_directive_flat_control, theater_ratio, 20, 0.22).
narrative_ontology:measurement(aneyoshi_tr_t40, aneyoshi_stone_directive_flat_control, theater_ratio, 40, 0.28).
narrative_ontology:measurement(aneyoshi_tr_t60, aneyoshi_stone_directive_flat_control, theater_ratio, 60, 0.35).
narrative_ontology:measurement(aneyoshi_tr_t80, aneyoshi_stone_directive_flat_control, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(aneyoshi_be_t0, aneyoshi_stone_directive_flat_control, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(aneyoshi_be_t20, aneyoshi_stone_directive_flat_control, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(aneyoshi_be_t40, aneyoshi_stone_directive_flat_control, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(aneyoshi_be_t60, aneyoshi_stone_directive_flat_control, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(aneyoshi_be_t80, aneyoshi_stone_directive_flat_control, base_extractiveness, 80, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_su_t0, aneyoshi_stone_directive_flat_control, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(aneyoshi_su_t20, aneyoshi_stone_directive_flat_control, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(aneyoshi_su_t40, aneyoshi_stone_directive_flat_control, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(aneyoshi_su_t60, aneyoshi_stone_directive_flat_control, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(aneyoshi_su_t80, aneyoshi_stone_directive_flat_control, suppression_requirement, 80, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive_flat_control, attachment_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_directive_flat_control, tsunami_hazard_zone_digitization).
narrative_ontology:affects_constraint(aneyoshi_stone_directive_flat_control, japanese_land_tenure_resilience).
narrative_ontology:affects_constraint(aneyoshi_stone_directive_flat_control, post_disaster_relocation_politics).

% DUAL FORMULATION NOTE:
% The stone directive is a single historical constraint with multiple structural interpretations (rope, snare, tangled rope, mountain, scaffold). This story captures the flat substrate without decomposing into readings. The constraint's persistence depends on the interlocking of three systems: (1) cultural transmission of disaster memory, (2) formal legal/administrative enforcement, (3) physical durability of the marker itself. If any system fails, the constraint's type changes. The network edges point to related constraints that amplify or substitute for this one: digitization of hazard zones (potential scaffold successor), land tenure institutions that carry the restriction forward (institutional substrate), and politics of post-disaster relocation (alternative mechanism for achieving the same protection goal).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_directive_flat_control, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
