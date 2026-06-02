% ============================================================================
% CONSTRAINT STORY: container_building_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_container_building_threshold, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: container_building_threshold
 *   human_readable: Container Building Threshold for Genuine Disclosure
 *   domain: social_psychology/gender_studies/relational_architecture
 *
 * SUMMARY:
 *   The container building threshold describes the structural requirement
 *   that genuine disclosure—vulnerability, emotional intimacy, authentic
 *   self-presentation—cannot occur in arbitrary social contexts but requires
 *   specific relational architectures built through identifiable
 *   technologies. Three primary pathways exist: (1) dissolution/art (extended
 *   unstructured time in liminal spaces—road trips, creative collaboration,
 *   psychedelic experiences—that dissolve normal social boundaries), (2)
 *   hosted regularity (consistent scheduled interaction in stable
 *   contexts—weekly dinners, standing meetups, ritual gatherings), and (3)
 *   shared activity (goal-oriented collaboration that builds containers as
 *   byproduct—team sports, creative projects, political organizing). Each
 *   technology operates on different timescales (dissolution/art:
 *   months-years; hosted regularity: weeks-months; shared activity:
 *   days-weeks) and exhibits different failure modes (premature
 *   structure-imposition, schedule collapse, goal-displacement respectively).
 *   The constraint exhibits genuine coordination function: these technologies
 *   do enable disclosure that wouldn't otherwise occur. But it also exhibits
 *   asymmetric extraction: the threshold systematically excludes those with
 *   least social capital (isolated individuals lack the resources to initiate
 *   container-building), least time (workers cannot sustain hosted
 *   regularity), and least geographic stability (mobile populations face
 *   repeated threshold-crossing costs). The extractiveness has increased over
 *   the interval (0.42 → 0.58) as social atomization, time poverty, and
 *   geographic mobility have risen, raising the effective threshold. Theater
 *   ratio has also increased (0.35 → 0.48) as performative substitutes
 *   (social media connection, professional networking, transactional
 *   socializing) have proliferated without delivering genuine disclosure,
 *   creating the appearance of container-building without the substance.
 *
 * KEY AGENTS:
 *   - Socially Isolated Individuals: Primary victim (powerless/trapped) — lack existing containers and cannot architect new ones alone; face catch-22 where building containers requires social resources isolation has depleted
 *   - Time-Poor Workers: Secondary victim (moderate/constrained) — can access some container technologies but dissolution/art pathway beyond reach; bear cost in time allocation and schedule coordination
 *   - Geographically Mobile Populations: Secondary victim (moderate/mobile) — face repeated threshold-crossing costs with each relocation; containers are non-portable, requiring rebuild in each new context
 *   - Container Architects: Primary beneficiary (institutional/arbitrage) — therapeutic professionals, community organizers, facilitators who possess skills and social capital to build containers efficiently; capture professional rents from specialized knowledge
 *   - Digital Community Infrastructure: Organized agents (organized/constrained) — building alternative pathways (online platforms, virtual communities) that reduce threshold; see traditional face-to-face requirement as temporary with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and asymmetric extraction; notes threshold is not minimized and could be lower with different social arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(container_building_threshold, 0.58).
domain_priors:suppression_score(container_building_threshold, 0.62).
domain_priors:theater_ratio(container_building_threshold, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(container_building_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(container_building_threshold, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(container_building_threshold, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(container_building_threshold, tangled_rope).
narrative_ontology:human_readable(container_building_threshold, "Container Building Threshold for Genuine Disclosure").
narrative_ontology:topic_domain(container_building_threshold, "social_psychology/gender_studies/relational_architecture").

domain_priors:requires_active_enforcement(container_building_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(container_building_threshold, container_architects).
narrative_ontology:constraint_beneficiary(container_building_threshold, therapeutic_professionals).
narrative_ontology:constraint_beneficiary(container_building_threshold, community_organizers).
narrative_ontology:constraint_victim(container_building_threshold, socially_isolated_individuals).
narrative_ontology:constraint_victim(container_building_threshold, time_poor_workers).
narrative_ontology:constraint_victim(container_building_threshold, geographically_mobile_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOCIALLY ISOLATED INDIVIDUAL (SNARE) — Trapped by lack of existing containers and inability to architect new ones alone. The constraint requires resources (time, social capital, emotional bandwidth) the isolated agent lacks. Experiences maximum extraction: needs disclosure containers to exit isolation, but building containers requires the very social resources isolation has depleted. Catch-22 structure.
constraint_indexing:constraint_classification(container_building_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TIME-POOR WORKER (TANGLED ROPE) — Constrained by competing demands but not fully trapped. Can access some container-building technologies (hosted regularity via scheduled meetups, shared activity via hobby groups) but dissolution/art pathway requires time investment beyond reach. Benefits from containers when successfully built (genuine connection, emotional support) but bears significant cost in time allocation and schedule coordination. Mixed experience: real coordination function exists alongside real extraction.
constraint_indexing:constraint_classification(container_building_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTAINER ARCHITECT (ROPE) — Therapeutic professionals, community organizers, group facilitators who possess the skills and social capital to build containers efficiently. Experiences the constraint as coordination: the technologies (dissolution/art, hosted regularity, shared activity) are tools that solve the legitimate problem of creating spaces for genuine disclosure. Net beneficiary through professional role, social status, and access to multiple container types. Low effective extraction because the constraint subsidizes their structural position.
constraint_indexing:constraint_classification(container_building_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GEOGRAPHICALLY MOBILE PROFESSIONAL (TANGLED ROPE) — Mobile exit options (can relocate, access multiple communities) but faces repeated container-building costs with each move. Benefits from container technologies when established (genuine connection in new location) but bears extraction through repeated threshold-crossing. The constraint coordinates disclosure in each new context but extracts through the non-portability of containers — each relocation resets to zero. Generational timeframe reflects career-long pattern of build-abandon-rebuild cycles.
constraint_indexing:constraint_classification(container_building_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DIGITAL COMMUNITY INFRASTRUCTURE (SCAFFOLD) — Online platforms, virtual communities, and digital container-building tools (Discord servers, online support groups, asynchronous forums) are creating alternative pathways that reduce the threshold for container formation. Organized agents building these infrastructures see the traditional face-to-face container requirement as a temporary coordination problem with a sunset: digital technologies enable container-building at lower time/geographic cost. Estimated sunset: 15-25 years as norms mature around virtual intimacy and digital containers achieve parity with physical ones for disclosure depth.
constraint_indexing:constraint_classification(container_building_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination function (container technologies do enable disclosure that wouldn't otherwise occur) and asymmetric extraction (the threshold systematically excludes those with least social capital). The three technologies operate on different timescales: dissolution/art requires extended unstructured time (months-years), hosted regularity requires medium-term commitment (weeks-months), shared activity can build containers faster (days-weeks) but with shallower initial depth. Failure modes differ: dissolution/art fails through premature structure-imposition, hosted regularity fails through schedule collapse, shared activity fails through goal-displacement. The analytical view sees this as tangled rope rather than pure coordination because the threshold is not minimized — it could be lower with different social arrangements, but current norms maintain extraction-enabling barriers.
constraint_indexing:constraint_classification(container_building_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(container_building_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(container_building_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(container_building_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(container_building_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(container_building_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The threshold systematically excludes those with least social capital, time, and geographic stability. While the constraint has genuine coordination function (containers do enable disclosure), the threshold is not minimized—it could be substantially lower. The increase from 0.42 to 0.58 over the interval reflects rising social atomization, time poverty, and geographic mobility, which raise the effective barrier. The extraction is not total (hence not snare from all perspectives) because some agents can build containers, and the technologies do work when accessible. Suppression (0.62): Moderate-high. Significant barriers include: lack of existing social capital to initiate container-building, time scarcity preventing sustained engagement, geographic mobility forcing repeated threshold-crossing, and cultural norms that stigmatize vulnerability outside established containers. Suppression is not total because alternatives exist (digital containers, therapeutic contexts, intentional communities), but these alternatives themselves require resources many lack. Theater ratio (0.48): Moderate. Performative substitutes have proliferated—social media 'connection,' professional networking, transactional socializing—that create the appearance of container-building without delivering genuine disclosure. The theater has increased as these substitutes have become normalized, but it's not dominant (hence not piton) because genuine container-building technologies still function when accessed. The ratio reflects that roughly half of apparent container-building activity is performative rather than functional.
 *
 * PERSPECTIVAL GAP:
 *   The container architect sees coordination (Rope)—the technologies solve the legitimate problem of creating spaces for genuine disclosure, and they benefit from professional expertise in applying these technologies. The socially isolated individual sees pure extraction (Snare)—trapped by lack of existing containers and inability to architect new ones alone, facing a catch-22 structure. The time-poor worker and geographically mobile professional see mixed coordination and extraction (Tangled Rope)—the technologies do work when accessible, providing real benefit, but the threshold extracts through time costs and non-portability. The digital infrastructure coalition sees a temporary problem with a sunset (Scaffold)—online platforms are creating alternative pathways that reduce the threshold, with an estimated 15-25 year timeline to functional parity. The analytical observer sees tangled rope at the civilizational level—genuine coordination function exists, but the threshold is not minimized and maintains extraction-enabling barriers that could be reduced with different social arrangements. The gap reveals that 'container-building is necessary for genuine disclosure' is simultaneously true (coordination function) and a mechanism of extraction (threshold systematically excludes the socially marginalized).
 *
 * DIRECTIONALITY LOGIC:
 *   Container architects (institutional/arbitrage + beneficiary) experience low directionality (d ≈ 0.10) because the constraint subsidizes their structural position—they possess the skills and social capital to build containers efficiently and capture professional rents from this specialized knowledge. Their effective extraction is negative (they extract from the system rather than being extracted from). Socially isolated individuals (powerless/trapped + victim) experience maximum directionality (d ≈ 0.95) because they lack the resources to build containers and face a catch-22: need containers to exit isolation, but building containers requires social resources isolation has depleted. Time-poor workers and geographically mobile populations (moderate/constrained + victim) experience high but not maximum directionality (d ≈ 0.70-0.80) because they have some agency and can access some container technologies, but face significant barriers. The digital infrastructure coalition (organized/constrained + beneficiary) experiences moderate directionality (d ≈ 0.45) because they are building alternatives that reduce the threshold, giving them agency and a coordination role, but they also face constraints in achieving adoption and norm change. The analytical observer (analytical/analytical + mixed) experiences moderate-high directionality (d ≈ 0.72) reflecting the standard analytical position that sees structure without being fully captured by any single perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the same structural phenomenon—the requirement for specific relational architectures to enable genuine disclosure—exhibits both coordination and extraction depending on the agent's structural position. The coordination function is real: containers do enable disclosure that wouldn't otherwise occur, and the three technologies (dissolution/art, hosted regularity, shared activity) are identifiable mechanisms that work. But the extraction is also real: the threshold systematically excludes those with least social capital, time, and geographic stability, and the threshold is not minimized—it could be lower. The tangled rope classification at the analytical level captures this duality: genuine coordination function coexists with asymmetric extraction. The constraint is not mislabeled coordination (it really does coordinate disclosure) and not pure extraction (the technologies really do work when accessible). The perspectival gap between the architect's rope and the isolated individual's snare is not a measurement error—it reflects the structural reality that the same constraint subsidizes some agents while extracting from others. The scaffold perspective from the digital infrastructure coalition adds a temporal dimension: the threshold may be temporary, with a sunset as virtual containers achieve parity with physical ones. The mandatrophy resolution is that all these perspectives are simultaneously valid readings of the same structural data, and the presheaf over the observation site—the full set of indexed classifications—is the complete answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_container_parity_timeline,
    'At what point do digital containers achieve functional parity with physical containers for genuine disclosure depth?',
    'Longitudinal comparison of disclosure depth metrics (self-reported intimacy, vulnerability expression, emotional support quality) between matched physical and digital containers; tracking of digital-native vs digital-migrated container effectiveness over time',
    'If parity achieved within 10 years: scaffold perspective confirmed, threshold extraction substantially reduced. If parity requires 25+ years or never achieved: digital infrastructure is supplementary rather than substitutive, threshold extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_container_parity_timeline, empirical, 'Timeline for digital containers to match physical container disclosure depth').

omega_variable(
    container_portability_mechanism,
    'Can container-building skills transfer across contexts, or is each container context-specific and non-portable?',
    'Tracking individuals across geographic/social relocations; measuring time-to-threshold in second/third containers vs first container; identifying transferable vs context-dependent container-building competencies',
    'If skills transfer: extraction is front-loaded (high initial cost, lower subsequent cost), reducing lifetime extraction for mobile populations. If non-transferable: extraction compounds with each relocation, increasing lifetime burden for mobile agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(container_portability_mechanism, empirical, 'Whether container-building skills are portable across contexts').

omega_variable(
    dissolution_art_necessity,
    'Is the dissolution/art pathway necessary for deepest disclosure, or can hosted regularity and shared activity achieve equivalent depth given sufficient time?',
    'Comparative depth analysis across container types controlling for time investment; identification of disclosure topics/vulnerability levels accessible only through dissolution/art vs achievable through other pathways',
    'If dissolution/art is necessary: the constraint has an irreducible high-cost pathway for deepest connection, justifying higher extractiveness. If other pathways sufficient: dissolution/art is a luxury good rather than a necessity, and the constraint''s coordination function is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissolution_art_necessity, empirical, 'Whether dissolution/art pathway is necessary for deepest disclosure').

omega_variable(
    threshold_vs_maintenance_cost_ratio,
    'What proportion of total container cost is threshold-crossing vs ongoing maintenance?',
    'Time-series analysis of effort investment in container relationships; identification of initial vs sustained effort patterns; comparison of threshold-crossing failure rates vs maintenance-phase failure rates',
    'If threshold dominates (>70% of total cost): the constraint is primarily about initial access barriers, confirming high extractiveness. If maintenance dominates: the constraint is more about sustained coordination, reducing extractiveness estimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_vs_maintenance_cost_ratio, empirical, 'Proportion of container cost in threshold-crossing vs maintenance').

omega_variable(
    architect_skill_distribution,
    'Is container-building skill normally distributed in the population, or is there a bimodal distribution with a small architect class and a large non-architect class?',
    'Population survey of container-building competencies; network analysis of who initiates vs who joins containers; identification of skill acquisition pathways and barriers',
    'If normally distributed: most people can learn container-building, reducing extraction (coordination problem). If bimodal: container-building is a specialized skill, increasing extraction (architect class captures rents from skill scarcity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architect_skill_distribution, empirical, 'Distribution of container-building skills in population').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(container_building_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(container_theater_t0, container_building_threshold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(container_theater_t3, container_building_threshold, theater_ratio, 3, 0.4).
narrative_ontology:measurement(container_theater_t6, container_building_threshold, theater_ratio, 6, 0.45).
narrative_ontology:measurement(container_theater_t10, container_building_threshold, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(container_extract_t0, container_building_threshold, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(container_extract_t3, container_building_threshold, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(container_extract_t6, container_building_threshold, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(container_extract_t10, container_building_threshold, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(container_building_threshold, attachment_coordination).
narrative_ontology:boltzmann_floor_override(container_building_threshold, 0.08).
narrative_ontology:affects_constraint(container_building_threshold, gendered_disclosure_asymmetry).
narrative_ontology:affects_constraint(container_building_threshold, extractive_disclosure_calibration).

% DUAL FORMULATION NOTE:
% The container building threshold is upstream of both gendered disclosure asymmetry (which describes how gender norms differentially allocate container-building labor) and extractive disclosure calibration (which describes how disclosure can be weaponized within containers). This constraint describes the threshold for container formation itself; the downstream constraints describe dynamics within and across containers once formed. The three constraints form a family: container_building_threshold (coordination + extraction in threshold-crossing) → gendered_disclosure_asymmetry (coordination in gender-differentiated container roles) + extractive_disclosure_calibration (extraction through weaponized disclosure). Each has its own ε value reflecting different structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
