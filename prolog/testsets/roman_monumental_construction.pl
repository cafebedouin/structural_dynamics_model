% ============================================================================
% CONSTRAINT STORY: roman_monumental_construction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_monumental_construction, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: roman_monumental_construction
 *   human_readable: The Roman State's Monopoly on Opus Caementicium Construction
 *   domain: socio_technological/imperial_infrastructure
 *
 * SUMMARY:
 *   The Roman monumental construction system (opus caementicium architecture)
 *   represents a hybrid socio-technical constraint that coordinates
 *   massive-scale engineering while extracting labor, materials, and wealth
 *   from provincial populations. From the 1st-2nd centuries CE through the
 *   3rd-century crisis, the system functioned as a genuine tangled rope: it
 *   solved a real coordination problem (how to build aqueducts, roads,
 *   forums, fortifications across continental scale) while simultaneously
 *   extracting coercive labor, mandated contributions, and political
 *   subordination. The constraint exhibits all six DR types from different
 *   observational positions: pure extraction (Snare) from the view of
 *   conscripted laborers and municipalities; pure coordination (Rope) from
 *   the master builders and imperial state; temporary coordination (Scaffold)
 *   from provincial elites managing expectations across generational cycles;
 *   degraded performance (Piton) from the late-empire administrative
 *   bureaucracy maintaining fictional grandeur as actual capacity declined;
 *   and apparent natural law (false Mountain) from the analytical perspective
 *   that conflates technological necessity with political choice. The theater
 *   ratio (0.55, rising to 0.65 by century 3) reflects the constraint's
 *   degradation: early imperial projects solved real problems and were
 *   genuinely functional; late imperial projects increasingly served as
 *   displays of imperial continuity even as execution quality declined and
 *   completion rates fell. The extractiveness trajectory (0.38 → 0.52 → 0.58)
 *   shows intensification: as economic capacity weakened in the later empire,
 *   the same coercive apparatus extracted proportionally more from a
 *   shrinking resource base, pushing the constraint toward pure extraction.
 *
 * KEY AGENTS:
 *   - Imperial State: Primary beneficiary (institutional/arbitrage) — captures prestige, infrastructure, political legitimacy; commands exit option over all projects
 *   - Master Builders Guild: Secondary beneficiary (organized/arbitrage) — specialized knowledge monopoly, high fees, cross-empire mobility
 *   - Material Suppliers: Secondary beneficiary (organized/constrained) — secure contracts, guaranteed demand, but constrained by imperial quality standards and price fixing
 *   - Provincial Municipalities: Primary victim (powerless/trapped) — mandatory labor contributions, material requisitions, tax obligations with no exit option
 *   - Construction Labor Force: Primary victim (moderate/constrained) — conscripted, enslaved, or debt-bonded workers; benefit from subsistence provisioning but suffer hazardous conditions and low autonomy
 *   - Provincial Elite: Secondary agent (organized/constrained) — manage upward extraction and downward control; experience shifts over generational timescale
 *   - Late-Empire Bureaucracy: Institutional actor (institutional/arbitrage) — maintain performative systems as functional capacity declines; see own process as increasingly theatrical
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing extractive coercion as technological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_monumental_construction, 0.52).
domain_priors:suppression_score(roman_monumental_construction, 0.68).
domain_priors:theater_ratio(roman_monumental_construction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_monumental_construction, extractiveness, 0.52).
narrative_ontology:constraint_metric(roman_monumental_construction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(roman_monumental_construction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_monumental_construction, tangled_rope).
narrative_ontology:human_readable(roman_monumental_construction, "The Roman State's Monopoly on Opus Caementicium Construction").
narrative_ontology:topic_domain(roman_monumental_construction, "socio_technological/imperial_infrastructure").

domain_priors:requires_active_enforcement(roman_monumental_construction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_monumental_construction, imperial_state).
narrative_ontology:constraint_beneficiary(roman_monumental_construction, master_builders).
narrative_ontology:constraint_beneficiary(roman_monumental_construction, material_suppliers).
narrative_ontology:constraint_victim(roman_monumental_construction, provincial_municipalities).
narrative_ontology:constraint_victim(roman_monumental_construction, private_landowners).
narrative_ontology:constraint_victim(roman_monumental_construction, slave_construction_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL MUNICIPALITY (SNARE) — Trapped by imperial mandate to contribute labor, materials, and funding for monumental projects. No exit option: refusal invites military reprisal or loss of civic status. Bears full extraction costs while benefiting minimally from projects often built to glorify remote emperors rather than serve local needs. Maximum experienced extraction due to trapped status and powerless position.
constraint_indexing:constraint_classification(roman_monumental_construction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: CONSTRUCTION LABOR FORCE (TANGLED ROPE) — Constrained by debt, legal status, and military conscription. Benefits from steady employment and subsistence provisioning during projects; also suffers extraction through labor control, low compensation, and hazardous conditions. Mixed experience: the organization of labor is partly coordination (large-scale project efficiency) and partly coercion (military discipline, slave status, debt bondage). Cannot fully exit but has some mobility across projects and regions.
constraint_indexing:constraint_classification(roman_monumental_construction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MASTER BUILDERS GUILD (ROPE) — Organized agents (architects, engineers, experienced contractors) who benefit substantially from the monopoly. They have arbitrage options: leverage their specialized knowledge to command high fees, move between provincial projects, or consult across the empire. The constraint is experienced as pure coordination — they solve a complex technical problem (monumental construction) and capture substantial rents without significant coercion. The guild's organization is enabled by the constraint, not hindered.
constraint_indexing:constraint_classification(roman_monumental_construction, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IMPERIAL STATE (ROPE) — Pure beneficiary experiencing the constraint as coordination mechanism. The state solves the monumental engineering problem and captures all surplus: prestige, infrastructure for military logistics, public works that legitimize imperial rule. Has complete exit option (arbitrage) — the state can commission or abandon projects at will. Net extractor — extraction runs toward the imperial institution, not away. The enforcement machinery (law, military, administrative apparatus) makes the constraint function, but from the state's perspective, this is coordination, not coercion.
constraint_indexing:constraint_classification(roman_monumental_construction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: PROVINCIAL ELITE (SCAFFOLD) — Organized but constrained. Local aristocrats delegate upward while maintaining local authority structures. They experience the constraint as temporary: as the empire consolidates (generational timescale), imperial projects become rationalized into predictable tax schedules rather than ad-hoc demands. Early empire (1st-2nd century) = high suppression, extractive theater. Later empire (3rd-4th century) = degraded theaters, declining project quality, higher costs for weaker returns. Sunset mechanism is the empire's fragmentation and decentralization of authority — as central power wanes, provincial control over resources reasserts.
constraint_indexing:constraint_classification(roman_monumental_construction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: MAINTENANCE BUREAUCRACY (PITON) — By the 3rd century crisis and beyond, the monumental construction constraint degrades into a performative system. Late-empire administrative apparatus maintains the fiction of grand projects (restoration mandates, architectural reports) even as actual construction and maintenance decline due to fiscal exhaustion. Theater ratio high (0.65+): bureaucratic theater persists via inertia while functional capacity atrophies. The system exists because alternatives haven't fully replaced it and because the imperial narrative still requires the appearance of permanent monumentality, not because it effectively delivers projects.
constraint_indexing:constraint_classification(roman_monumental_construction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, monumental construction is constrained by sheer physics: coordinating tens of thousands of workers, supplying materials, and maintaining engineering knowledge requires centralized authority. The Roman state is solving an inherent problem of scale — no way to build aqueducts, roads, coliseums without centralized power. This perspective risks naturalizing what is actually a contingent political-economic arrangement. However, the structural data shows beneficiaries (state, guilds) and victims (municipalities, labor) — revealing that 'inherent to scale' naturalizes extractive coercion as technological necessity.
constraint_indexing:constraint_classification(roman_monumental_construction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_monumental_construction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_monumental_construction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_monumental_construction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_monumental_construction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_monumental_construction, TR),
    TR >= 0.70.

:- end_tests(roman_monumental_construction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant resources (labor, materials, wealth) from provincial populations with limited direct return value. However, the value is not as extreme as pure extraction (0.66+) because some local infrastructure benefits are genuine — aqueducts do supply water, roads do reduce transport costs, though these are secondary to imperial strategic and prestige objectives. Suppression (0.68): High. Significant barriers to refusal include military reprisal, loss of civic status, legal penalties, and monopolization of large-scale construction by the imperial system. Private alternatives are severely constrained by law and imperial authority. Theater ratio (0.55): Moderate-high and rising. Early-empire projects are primarily functional (aqueducts work, roads connect, fortifications defend). By century 3, maintenance and new projects become increasingly performative — the imperial bureaucracy reports grandeur while actual completion rates and quality decline. Claimed type (Tangled Rope): The constraint exhibits genuine coordination (solving large-scale engineering) and asymmetric extraction (benefiting state/guilds, harming provinces/labor). Both features are required and active.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across a six-type spectrum. The imperial state and master builders see a pure coordination solution (Rope) — they capture the technical problem-solving value. Municipalities and conscripted laborers see pure extraction (Snare) — they bear costs with no strategic benefit. Organized provincial elites see a temporary arrangement (Scaffold) — they manage extraction predictably across generational cycles, betting that centralization will eventually stabilize or decline. The late-empire bureaucracy sees its own degraded system (Piton) — maintaining fictional grandeur as real capacity atrophies. The construction labor force experiences mixed coordination and coercion (Tangled Rope) — the system both organizes complex work and constrains their agency. The civilizational analytical observer risks seeing technological necessity (Mountain) — large-scale engineering requires centralized power — but the structural data reveals this is naturalizing: alternative funding and labor models were available; coercion was political choice, not physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position: beneficiaries with arbitrage options (imperial state, master builders) experience low d (0.05-0.15), producing negative effective extraction via the sigmoid f(d). Victims with trapped exits (municipalities, conscripted labor) experience high d (0.85-0.95), producing high effective extraction. Moderately constrained agents (construction labor with some project mobility) experience middle d (0.55-0.65), producing moderate f(d) values. The piton perspective derives from high theater ratio and low functional improvement, not from high extraction chi. The false mountain perspective is revealed by the structural beneficiary/victim data: the constraint is contingent political-economic arrangement, not inherent to scale itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the six types reflect genuine perspectival differences, not measurement ambiguity. The key resolving observation: beneficiaries and victims are real and distinct (not symmetric actors), enforcement is active (imperial military, legal apparatus), and the constraint exhibits both coordination function (solving engineering problems) and extraction (asymmetric distribution of costs/benefits). This makes Tangled Rope the correct claimed type at the analytical level. The Snare perspective (from powerless municipalities) is valid for those agents. The Rope perspective (from imperial state) is valid for that agent. The Scaffold perspective is valid for organized provincial elites managing across time. The Piton perspective emerges in late empire as the system's functional capacity degrades while administrative theater persists. The false Mountain perspective reveals the error of naturalizing contingent political-economic coercion as inherent technological necessity. The presheaf of perspectives IS the complete answer — no single type captures the constraint; the indexed family does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    local_voluntary_participation,
    'What fraction of monumental construction labor was genuinely voluntary (hired market labor) vs. coerced (slave, conscripted, or debt-bonded)?',
    'Archaeological and epigraphic analysis of wage records, military conscription documents, and legal codes. Comparison of voluntary vs. coerced worker percentages by project and era.',
    'If >50% voluntary: constraint shifts toward Rope (coordination mechanism). If <30% voluntary: constraint solidifies as Snare from labor perspective. Distribution across eras reveals whether empire became more extractive or more consensual over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_voluntary_participation, empirical, 'Ratio of voluntary to coerced labor in monumental construction').

omega_variable(
    provincial_benefit_realization,
    'Did provinces genuinely benefit from monumental projects through improved infrastructure (water, transport, defense), or were projects primarily extractive displays of imperial power with minimal local utility?',
    'Archaeological data on aqueduct functionality, road maintenance, defensive efficacy, and longevity. Economic analysis of trade flow changes post-construction. Local epigraphic evidence of provincial sentiment and adoption.',
    'If high benefit realization: constraint appears more as Tangled Rope from provincial perspective (some coordination benefit mitigates extraction). If low benefit: constraint is pure Snare — extraction with no return value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(provincial_benefit_realization, empirical, 'Magnitude of actual provincial benefit from monumental projects').

omega_variable(
    alternative_funding_mechanisms,
    'Could monumental construction have been financed through voluntary market mechanisms, or was centralized coercion intrinsic to Roman-era scaling?',
    'Comparative analysis: private construction in Late Antique cities vs. imperial era. Economic modeling of voluntary market financing for large projects. Analysis of successful non-state monumental projects (temples, theaters funded by wealthy individuals).',
    'If viable alternatives existed: the coercion was contingent, not inherent — constraint shifts toward Snare/Tangled Rope. If alternatives were infeasible: constraint approaches Mountain (technological necessity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_funding_mechanisms, conceptual, 'Whether non-coercive financing mechanisms were viable').

omega_variable(
    late_empire_degradation_mechanism,
    'Does the 3rd-4th century shift toward Piton classification represent genuine atrophy (functional capacity declining while theater persists) or recalibration toward smaller, more sustainable projects?',
    'Comparison of project scale, completion rates, and maintenance quality (1st-2nd century vs. 3rd-4th century). Analysis of architectural styles, material costs, and labor recruitment methods. Epigraphic evidence of imperial mandates vs. actual construction activity.',
    'If genuine atrophy: Piton classification confirmed — the system persists through narrative momentum as real capacity declines. If recalibration: the constraint evolves toward Scaffold (purposeful downsizing, not degradation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_empire_degradation_mechanism, empirical, 'Nature of Late Antique construction system changes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_monumental_construction, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(romc_tr_t0, roman_monumental_construction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(romc_tr_t50, roman_monumental_construction, theater_ratio, 50, 0.55).
narrative_ontology:measurement(romc_tr_t100, roman_monumental_construction, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(romc_be_t0, roman_monumental_construction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(romc_be_t50, roman_monumental_construction, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(romc_be_t100, roman_monumental_construction, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_monumental_construction, resource_allocation).
narrative_ontology:affects_constraint(roman_monumental_construction, roman_debt_bondage).
narrative_ontology:affects_constraint(roman_monumental_construction, provincial_tax_collection).

% DUAL FORMULATION NOTE:
% Roman monumental construction is downstream of resource extraction mechanisms (taxation, forced labor recruitment) and upstream of infrastructure effects (trade efficiency, defensive capacity, legitimation narratives). This story focuses on the socio-technical constraint of organizing massive-scale opus caementicium projects. Related constraints (debt bondage, provincial taxation) have distinct extractiveness values reflecting their structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_monumental_construction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
