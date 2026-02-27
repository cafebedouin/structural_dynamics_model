% ============================================================================
% CONSTRAINT STORY: ulysses_chp10
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp10, []).

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
 *   constraint_id: ulysses_chp10
 *   human_readable: The Dublin Simultaneity (Wandering Rocks)
 *   domain: social/political/religious
 *
 * SUMMARY:
 *   Chapter 10 of Ulysses ('Wandering Rocks') presents Dublin on June 16,
 *   1904, as a system of simultaneous movements and near-misses. Multiple
 *   characters trace through the city — postal workers, priests, beggars,
 *   shopkeepers, officials — on intersecting trajectories that almost but
 *   never quite converge. The chapter's primary structural feature is
 *   simultaneity: the reader sees multiple events happening at the same
 *   moment, separated by geography, mapping the city's circulation patterns.
 *   This constraint exhibits all six DR types depending on perspective. The
 *   simultaneity appears as an immutable law of bounded urban space
 *   (Mountain), a coordination mechanism that enables institutional and
 *   commercial order (Rope), a mixed system of coordination and extraction
 *   (Tangled Rope), a degraded colonial administrative tool maintained
 *   through ritual (Piton), a temporary social form being transcended by
 *   modernist literary technique (Scaffold), and pure extraction for those
 *   trapped in fixed roles (Snare). The constraint's theater_ratio (0.68)
 *   reflects the performative aspects of Dublin's social geometry: ceremonial
 *   processions, official routes, sanctioned gathering places, and the
 *   choreography of encounters maintain a visible social order. The
 *   extractiveness has increased from 0.22 to 0.38 over the interval,
 *   indicating that the cost of immobility (the burden of being trapped in
 *   fixed social positions) has become more salient — what was once accepted
 *   as natural social order is increasingly experienced as constraint.
 *
 * KEY AGENTS:
 *   - Street beggars and servants: Primary victims (powerless/trapped) — have no exit from fixed geographic and social slots
 *   - Church and institutional authorities: Primary beneficiaries (institutional/arbitrage) — benefit from predictable mapping of congregants and subjects
 *   - Middle-class professionals and shopkeepers: Secondary participants (moderate/constrained) — constrained by business logic but also benefit from predictable customer and social circulation
 *   - Postal workers and infrastructure actors: Secondary victims (moderate/constrained) — enforcing the constraint through their own routinized movements
 *   - Modernist literary movement: Organized agents (organized/mobile) — see the constraint as temporary, document it as a social form in transition
 *   - Colonial administrative structures: Institutional maintainers (institutional/constrained) — use spatial and temporal organization for surveillance and control, but the function has degraded
 *   - Analytical observer (civilizational/universal): Risks naturalizing contingent social arrangements as immutable laws of physics or urban structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp10, 0.38).
domain_priors:suppression_score(ulysses_chp10, 0.52).
domain_priors:theater_ratio(ulysses_chp10, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp10, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp10, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ulysses_chp10, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp10, tangled_rope).
narrative_ontology:human_readable(ulysses_chp10, "The Dublin Simultaneity (Wandering Rocks)").
narrative_ontology:topic_domain(ulysses_chp10, "social/political/religious").

domain_priors:requires_active_enforcement(ulysses_chp10).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp10, institutional_order).
narrative_ontology:constraint_beneficiary(ulysses_chp10, social_hierarchy_maintainers).
narrative_ontology:constraint_victim(ulysses_chp10, marginal_actors).
narrative_ontology:constraint_victim(ulysses_chp10, cross_class_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STREET BEGGAR/SERVANT (SNARE) — Trapped in Dublin's social geography; cannot exit predefined routes or economic positions. The simultaneity constraint enforces fixed spatial and temporal slots. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.43.
constraint_indexing:constraint_classification(ulysses_chp10, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE-CLASS SHOPKEEPER/PROFESSIONAL (TANGLED ROPE) — Constrained by business hours, professional obligations, and social expectations about movement through the city. Also benefits from predictable circulation patterns that enable commerce and professional networks. d≈0.58, f(d)≈0.71, σ=0.8 → χ≈0.21.
constraint_indexing:constraint_classification(ulysses_chp10, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: CHURCH AND SOCIAL INSTITUTIONS (ROPE) — Benefits from the simultaneity constraint as coordination mechanism. The constraint enables predictable mapping of social positions, institutional authority, and collective rituals. Church knows where congregants will be at what times. d≈0.12, f(d)≈0.08, σ=0.8 → χ≈0.003. Near-zero extraction; pure coordination benefit.
constraint_indexing:constraint_classification(ulysses_chp10, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: MODERNIST LITERARY MOVEMENT (SCAFFOLD) — Organized agents (Joyce, literary circles) see the simultaneity constraint as a temporary social form to be documented, decomposed, and transcended through narrative technique. The constraint has a sunset: urbanization and technological acceleration are dissolving the fixed simultaneity of pre-automobile Dublin. d≈0.35, f(d)≈0.31, σ=0.8 → χ≈0.08.
constraint_indexing:constraint_classification(ulysses_chp10, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: COLONIAL ADMINISTRATIVE STRUCTURE (PITON) — British institutional control over Ireland operates partly through spatial and temporal organization of Dublin: predictable flows enable surveillance, tax collection, and control of sedition. The theater_ratio=0.68 reflects performative aspects of the constraint: the ceremonial geography of Dublin (Lord Lieutenant's routes, official processions, sanctioned gathering places) is maintained through ritual more than effective coercion. The primary function (colonial administration) has atrophied; the constraint persists through institutional inertia.
constraint_indexing:constraint_classification(ulysses_chp10, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, simultaneity is an immutable constraint of physical and social systems: bodies cannot occupy multiple locations; events occur in sequence or overlap. The Dublin simultaneity reflects an invariant property of how collective life is organized in bounded geographic space. However, the base properties (ε=0.38, suppression=0.52, theater=0.68) contradict the mountain classification — this is a false summit. The constraint is contingent on pre-industrial transportation, fixed social hierarchies, and lack of communication technologies that dissolve simultaneity.
constraint_indexing:constraint_classification(ulysses_chp10, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp10_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp10, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp10, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp10, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp10_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The simultaneity constraint extracts time, mobility, and agency from those trapped in fixed roles (beggars, servants, factory workers) while enabling commercial and institutional benefit for those who can arbitrage the predictable flows (merchants, clergy, administrators). The extraction is real but not as severe as a snare would suggest — much of the constraint is genuinely coordination. However, the burden of immobility for the powerless is substantial. The upward trend from 0.22 to 0.38 suggests that as the constraint becomes more visible (through narrative representation and modernist decomposition), its extractive dimension becomes more salient. Suppression (0.52): Moderate-high. The constraint is maintained through social custom, institutional authority, economic necessity, and lack of alternative transportation technology. However, suppression is not absolute — some agents (modernist writers, emerging middle class) are developing exit strategies through educational mobility, literary technique, and technological change. Theater ratio (0.68): High and increasing. The constraint is maintained partly through performative ritual: ceremonial processions, official routes, sanctioned public spaces. The ceremonial geography of Dublin (Lord Lieutenant's processional paths, church rituals, market day rhythms) is performative — it maintains social visibility and symbolic order. The theater increases over the interval as the underlying structural need for the constraint (transportation bottleneck, communications lag) declines, making the performative maintenance more necessary to sustain the social form.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how a single structural phenomenon (the simultaneity of Dublin's circulation patterns) generates radically different classifications depending on observational position. The street beggar sees pure extraction (Snare) — trapped in fixed economic roles, bearing the full cost of immobility. The middle-class professional sees coordination with constraints (Tangled Rope) — the simultaneity enables business networks but limits mobility. The church sees coordination only (Rope) — the constraint enables the mapping and management of congregants without extractive burden. The modernist literary movement sees a temporary social form with a sunset (Scaffold) — urbanization, motorization, and communication technology are dissolving the fixed simultaneity. The colonial administrative structure sees its own degraded function (Piton) — the constraint was designed for control and surveillance, but the primary function has atrophied; the ritual persists through inertia. The analytical observer risks seeing an immutable natural law (Mountain) — simultaneity is inherent to bounded space — but the structural data contradicts this: the constraint is contingent on pre-industrial transportation, class stratification, and institutional territoriality.
 *
 * DIRECTIONALITY LOGIC:
 *   Street beggar/servant: Victim + trapped → d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.43. Maximum extraction with high directionality. Constrained middle-class: Victim + constrained → d≈0.58, f(d)≈0.71, σ=0.8 → χ≈0.21. Partial extraction; some beneficiary elements from commercial coordination. Church/institutional: Beneficiary + arbitrage → d≈0.12, f(d)≈0.08, σ=0.8 → χ≈0.003. Near-zero extraction; pure coordination benefit. Modernist movement: Organized + mobile → d≈0.35, f(d)≈0.31, σ=0.8 → χ≈0.08. Low extraction; coalition has exit paths. Colonial administration: Institutional + constrained → d≈0.40, f(d)≈0.40, σ=0.8 → χ≈0.12. Moderate extraction but primarily performative (piton gate). Analytical observer: analytical → d≈0.72, f(d)≈1.15, σ=0.8 → χ≈0.35. Mountain classification is perspectival risk — observer naturalizes contingent constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   LITERARY CASE STUDY: Joyce's Chapter 10 resolves the mandatrophy by making visible all six types simultaneously. The literary technique (presenting multiple simultaneous viewpoints and near-misses) is the engine for revealing the constraint's multiplicity. Mandatrophy in this context is the temptation to reduce Dublin's simultaneity to a single type: either 'merely coordination' (Rope, erasing the extraction from the powerless) or 'purely extractive' (Snare, erasing the genuine coordination functions). The text's solution is to show that the same constraint is Rope from the church's perspective, Snare from the beggar's perspective, Tangled Rope from the professional's perspective, and Piton from the colonial administrator's perspective. The constraint is real (not reducible to observer perspective alone), but its type is irreducibly indexical. The false summit (Mountain) is the risk of naturalizing this contingent social form as an immutable law of urban life — a risk that becomes salient only when the analytical observer asks whether pre-industrial simultaneity is necessary or contingent on specific technologies and hierarchies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simultaneity_vs_coincidence,
    'Is the constraint structural simultaneity (bodies moving through fixed routes at predictable times) or merely narrative coincidence (the literary technique that makes near-misses visible)?',
    'Historical analysis of actual Dublin movement patterns in 1904; comparison of Joyce''s chapter against documented routes of postal workers, tram schedules, institutional processionals; assessment of whether actual near-misses match the literary structure or are narrative invention',
    'If structural: constraint is a real social form (Tangled Rope). If narrative: constraint is a literary device projected onto the city (Piton — performative). The classification hinges on whether the simultaneity exists in Dublin''s actual circulation or only in the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simultaneity_vs_coincidence, empirical, 'Whether simultaneity is structural or narrative').

omega_variable(
    extraction_mechanism_clarity,
    'What specifically is being extracted from whom? Is it time, mobility, agency, predictability of position, or control over narrative self-presentation?',
    'Structural analysis of who benefits from fixed social positions and predictable routes; who pays costs for immobility; tracing of resource flows and opportunity asymmetries correlating with the simultaneity constraint',
    'If extractive mechanism is clear and measurable: Tangled Rope classification confirmed. If extraction is metaphorical or unquantifiable: constraint is better modeled as Rope (pure coordination) or Piton (theater with degraded extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_clarity, conceptual, 'Clarity of the extraction mechanism beneath simultaneity').

omega_variable(
    colonial_vs_urban_causation,
    'Is the constraint primarily imposed by colonial administrative structures, or does it emerge from pre-industrial urban geography itself (population density, transportation technology, market dynamics)?',
    'Historical comparison of Dublin''s simultaneous movements to other pre-industrial cities without British colonial rule; analysis of how simultaneity patterns changed post-independence and post-motorization; attribution of causal factors to colonial vs. urban-structural variables',
    'If colonial-caused: the Piton perspective (degraded administrative control) is primary. If urban-structural: the Rope perspective (coordination) or Mountain perspective (inevitable property of bounded cities) is primary. The classification cluster shifts depending on causation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_vs_urban_causation, empirical, 'Whether constraint is caused by colonialism or urban structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp10, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rocks_tr_t0, ulysses_chp10, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rocks_tr_t5, ulysses_chp10, theater_ratio, 5, 0.58).
narrative_ontology:measurement(rocks_tr_t10, ulysses_chp10, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(rocks_be_t0, ulysses_chp10, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(rocks_be_t5, ulysses_chp10, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(rocks_be_t10, ulysses_chp10, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp10, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp10, irish_colonial_territoriality).
narrative_ontology:affects_constraint(ulysses_chp10, dublin_class_segregation).
narrative_ontology:affects_constraint(ulysses_chp10, modernist_narrative_technique).

% DUAL FORMULATION NOTE:
% The Dublin simultaneity is decomposed into three related constraints: (1) colonial administrative use of spatial/temporal organization (Piton, ε≈0.25); (2) class-based segregation of movement routes (Tangled Rope, ε≈0.38, this story); (3) the literary technique that makes the simultaneity visible (Scaffold, ε≈0.15). The ε values reflect distinct structural phenomena: degraded colonial control, actual extraction from the powerless, and the emerging modernist response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp10, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
