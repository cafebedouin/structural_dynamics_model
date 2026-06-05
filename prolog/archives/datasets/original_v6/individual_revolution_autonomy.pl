% ============================================================================
% CONSTRAINT STORY: individual_revolution_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_revolution_autonomy, []).

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
 *   constraint_id: individual_revolution_autonomy
 *   human_readable: The One-Man Revolution: Individual Autonomy vs. Mass Collective Action
 *   domain: political/social
 *
 * SUMMARY:
 *   The constraint 'The One-Man Revolution' encodes a fundamental tension in
 *   contemporary political life: the simultaneous celebration of individual
 *   autonomy and the delegitimization of mass collective action. This
 *   constraint creates a structural incentive for atomized individuals to
 *   pursue personal, local, or self-improvement projects while treating
 *   systemic socio-political transformation as inherently futile. The
 *   constraint is neither purely natural nor purely imposed — it emerges from
 *   the intersection of historical institutional arrangements, psychological
 *   incentive structures, technological affordances that enable individual
 *   action but fragment collective coordination, and explicit ideological
 *   framing by both establishment and post-ideological actors. The constraint
 *   exhibits all six DR types from different perspectives, revealing it as a
 *   sophisticated hybrid coordination-extraction mechanism masquerading as
 *   either natural law (mountain) or liberatory autonomy (rope). From the
 *   perspective of atomized masses, it is a snare: collective mobilization
 *   capacity is suppressed while individual pursuits are celebrated, leaving
 *   populations more fragmented and weaker relative to institutional power.
 *   From the perspective of institutional actors, it is a rope: a
 *   coordination mechanism that solves the problem of mass action without
 *   requiring explicit repression. From the perspective of organized
 *   reformers, it is a scaffold: a temporary default that organized agents
 *   can transcend through building alternative coordination infrastructure.
 *   The constraint's theater ratio (0.68) reflects the performative nature of
 *   autonomy rhetoric — endless cultural celebration of 'doing your own
 *   thing' and 'personal revolution' masks the structural incapacity for
 *   coordinated transformation.
 *
 * KEY AGENTS:
 *   - Atomized Masses: Primary victim (powerless/trapped) — bear the cost of demobilization and fragmentation; lack collective power despite numerical majority
 *   - Institutional Power Structures: Primary beneficiary (institutional/arbitrage) — benefit from coordination mechanism that preserves hierarchy without explicit force; can adjust tolerance thresholds
 *   - Independent Reformers: Secondary actor (moderate/constrained) — experience mixed benefits and costs; genuine autonomy gains offset by loss of collective scale
 *   - Counter-Mobilization Movement: Organized agents (organized/constrained) — see atomization as temporary and buildable-over; working toward sunset of the constraint
 *   - Revolutionary Left Establishment: Institutional actor (organized/arbitrage) — maintains revolutionary rhetoric as theater while actual mobilization capacity has degraded
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as human psychological constants
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_revolution_autonomy, 0.55).
domain_priors:suppression_score(individual_revolution_autonomy, 0.65).
domain_priors:theater_ratio(individual_revolution_autonomy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_revolution_autonomy, extractiveness, 0.55).
narrative_ontology:constraint_metric(individual_revolution_autonomy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(individual_revolution_autonomy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_revolution_autonomy, tangled_rope).
narrative_ontology:human_readable(individual_revolution_autonomy, "The One-Man Revolution: Individual Autonomy vs. Mass Collective Action").
narrative_ontology:topic_domain(individual_revolution_autonomy, "political/social").

domain_priors:requires_active_enforcement(individual_revolution_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_revolution_autonomy, autonomous_individuals).
narrative_ontology:constraint_beneficiary(individual_revolution_autonomy, institutional_power_structures).
narrative_ontology:constraint_victim(individual_revolution_autonomy, collective_mobilization).
narrative_ontology:constraint_victim(individual_revolution_autonomy, mass_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MOBILIZED MASSES (SNARE) — Trapped in the belief that mass collective action is futile; bear the cost of demobilization through loss of collective power. Cannot exit the framing that individual autonomy is the only viable path. Experiences maximum extraction: collective capacity for change is suppressed while individual pursuits are celebrated, leaving the powerless more fragmented and weaker.
constraint_indexing:constraint_classification(individual_revolution_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL POWER STRUCTURES (ROPE) — Benefit from the constraint as a coordination mechanism: dispersing collective action into individual autonomy projects leaves institutional authority unchallenged and preserves existing hierarchies. Net beneficiary with exit options — can adjust repression/tolerance as needed. The constraint solves the institutional problem of mass coordination without requiring explicit force.
constraint_indexing:constraint_classification(individual_revolution_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE INDEPENDENT REFORMER (TANGLED ROPE) — Constrained by resource limitations and social isolation, yet experiences genuine benefits from the autonomy framing: can pursue meaningful personal and local change without mass coordination overhead. Mixed experience: extraction through limitation of scale, coordination benefit through reduced friction. Neither pure extraction nor pure coordination — requires both beneficiary status (autonomy gains) and victim status (isolation from collective power).
constraint_indexing:constraint_classification(individual_revolution_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE COUNTER-MOBILIZATION MOVEMENT (SCAFFOLD) — Organized agents (mutual aid networks, community organizing, digital platforms enabling collective action) see individual autonomy as a temporary default that can be transcended through building alternative coordination infrastructure. The constraint has a sunset: as networks mature, mass coordination becomes technically feasible again, and the 'futile revolution' narrative loses force. Suppression rationale: current institutional barriers are high, but organized agents see them as removable through structural rebuilding.
constraint_indexing:constraint_classification(individual_revolution_autonomy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE REVOLUTIONARY LEFT ESTABLISHMENT (PITON) — Institutional actors (political parties, established NGOs, union hierarchies) that once mobilized mass movements now celebrate individual autonomy as the new orthodoxy. Their revolutionary rhetoric persists as theater while their functional capacity for mass coordination has atrophied. They maintain institutional form while abandoning institutional function. Theater ratio high: mobilization language persists in brand and rhetoric while actual mobilization capacity is degraded, preserved only through organizational inertia and nostalgic performance.
constraint_indexing:constraint_classification(individual_revolution_autonomy, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, human psychology and history suggest that mass coordination of atomized individuals is inherently difficult or impossible: individual action is psychologically immediate and rewarding, while collective action requires sacrifice of autonomy for delayed, uncertain, distributed rewards. The constraint reflects an immutable fact about human motivation. However, structural data reveals this as false naturalization: the high suppression and theater values show that the constraint is actively maintained, not naturally emergent. The analytical engine will identify this as a false summit.
constraint_indexing:constraint_classification(individual_revolution_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_revolution_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_revolution_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_revolution_autonomy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_revolution_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(individual_revolution_autonomy, TR),
    TR >= 0.70.

:- end_tests(individual_revolution_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The constraint extracts collective power from atomized individuals and redistributes it to institutional actors through a coordination mechanism rather than through explicit coercion. The extraction is real — individuals lose the capacity for mass transformation — but is offset by genuine autonomy gains in personal and local projects. The value reflects the hybrid nature: significant extraction of collective capacity, but not totalizing. Suppression (0.65): High. Significant barriers include psychological incentive structures (individual action is immediately rewarding; collective action requires delayed, distributed rewards), technological fragmentation (platforms designed for individual expression vs. mass coordination), institutional discouragement of mass action (permits, surveillance, infiltration), and cultural narratives that celebrate autonomy while dismissing collective action as utopian or totalitarian. Theater ratio (0.68): High. The autonomy rhetoric is substantially performative — endless celebration of 'personal revolution' and 'individual agency' masks the structural incapacity for coordinated transformation. The theater has increased over the interval as the autonomy framing has become more aestheticized and less connected to actual power asymmetries.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates profound perspectival divergence. Institutional actors experience it as coordination (Rope) — solving the problem of mass mobilization through atomization. The powerless experience it as extraction (Snare) — their collective power is suppressed. Organized reformers experience it as a temporary structure with a sunset (Scaffold) — buildable-over through alternative coordination infrastructure. Independent reformers experience mixed coordination and extraction (Tangled Rope) — genuine autonomy gains offset by isolation from collective scale. The revolutionary establishment experiences degradation (Piton) — maintaining revolutionary language as theater while actual capacity has atrophied. The analytical observer risks naturalizing the constraint as immutable law (Mountain) — individual action is psychologically immediate, collective action inherently difficult — but the structural data reveals this as false: the high suppression and theater values show active maintenance, not natural emergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position relative to the extraction mechanism. Institutional beneficiaries experience low d (0.15-0.25): they benefit from atomization without being target of extraction; the constraint runs toward them. Atomized masses experience high d (0.85-0.95): they are primary targets; collective capacity is extracted from them. Independent reformers experience moderate d (0.50-0.60): they gain genuine autonomy benefits but lose collective scale. Organized agents experience constrained d (0.45-0.55): they have agency and exit paths, but current constraints are real. The pipeline computes effective extractiveness (chi) by multiplying base extractiveness by the sigmoid f(d) and scope modifier — institutional beneficiaries see low chi; atomized masses see high chi; the perspectival gap emerges from these different d values acting on the same base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely a Tangled Rope: it possesses BOTH a coordination function (solving the institutional problem of mass mobilization) AND asymmetric extraction (concentrating power by preventing collective action). The false mountain classification (naturalizing autonomy difficulty as immutable human nature) is unmasked by the high suppression and theater values — these indicate active maintenance, not natural emergence. The false rope classification (pure coordination with no extraction) ignores the asymmetric costs: institutional actors benefit without sacrifice, while atomized masses lose collective power. The tangled rope classification correctly captures that the constraint is genuinely useful for coordination AND genuinely extractive — the two functions are inseparable. The suppression and theater metrics reveal that the extraction is hidden behind coordination language and autonomy rhetoric. Resolution: the constraint is best understood as an institutional solution to the problem of post-ideological governance — rather than explicitly forbid collective action, fragment it into individual pursuits while celebrating fragmentation as liberation. This is more stable than pure repression (lower theater costs) and more efficient than explicit coordination (lower enforcement costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_coordination_capability,
    'Under what conditions can mass coordination emerge from atomized individuals without requiring pre-existing institutional infrastructure or centralized leadership?',
    'Historical analysis of successful mass mobilizations (color revolutions, Arab Spring, Hong Kong protests); longitudinal case studies of digital-enabled coordination vs. institutional-enabled coordination',
    'If coordination possible: the constraint is institutional capture (Tangled Rope from more perspectives). If inherently difficult: the constraint is closer to mountain or snare depending on whether difficulty is natural or enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_coordination_capability, empirical, 'Whether mass coordination can emerge without institutional scaffolding').

omega_variable(
    autonomy_framing_adoption,
    'To what degree do atomized individuals internalize the ''individual autonomy as futile revolution'' narrative through cultural internalization vs. structural coercion vs. rational calculation?',
    'Survey and ethnographic data on motivation for individual vs. collective action; cross-cultural comparison of autonomy framing adoption rates; historical tracking of how the autonomy narrative became dominant',
    'If primarily internalized: suppression value is high because coercion is psychological rather than physical. If primarily rational: suppression is lower because individuals retain option to reassess. If primarily coercive: suppression reflects real barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_framing_adoption, empirical, 'Mechanism of autonomy narrative adoption among atomized populations').

omega_variable(
    institutional_stability_dependence,
    'How much of the institutional benefit from the autonomy constraint depends on continued demobilization vs. active repression capacity?',
    'Comparative institutional analysis: correlation between institutional stability and active suppression mechanisms vs. passive atomization; historical cases where atomization without repression maintained stability',
    'If high dependence on active repression: institutional actors cannot relax the constraint without risking mobilization (high extractiveness, high suppression). If low dependence: institutions may allow coordination to resume if it doesn''t threaten core interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_stability_dependence, empirical, 'Institutional dependence on active suppression vs. passive atomization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_revolution_autonomy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indrev_tr_t0, individual_revolution_autonomy, theater_ratio, 0, 0.45).
narrative_ontology:measurement(indrev_tr_t15, individual_revolution_autonomy, theater_ratio, 15, 0.62).
narrative_ontology:measurement(indrev_tr_t30, individual_revolution_autonomy, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(indrev_be_t0, individual_revolution_autonomy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(indrev_be_t15, individual_revolution_autonomy, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(indrev_be_t30, individual_revolution_autonomy, base_extractiveness, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_revolution_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(individual_revolution_autonomy, mass_mobilization_capacity).
narrative_ontology:affects_constraint(individual_revolution_autonomy, institutional_legitimacy_without_consent).
narrative_ontology:affects_constraint(individual_revolution_autonomy, post_ideological_governance).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific institutional arrangements (regulatory capture, surveillance states, platform governance) but represents a distinct structural pattern: the atomization of collective action capacity through celebration of individual autonomy. The upstream constraint (institutional governance mechanisms) depends on the autonomy framing to remain stable; the downstream constraints (specific extraction mechanisms) rely on the atomized state to avoid triggering collective response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(individual_revolution_autonomy, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
