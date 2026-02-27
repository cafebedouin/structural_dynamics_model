% ============================================================================
% CONSTRAINT STORY: fraser_river_salmon_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fraser_river_salmon_regulation, []).

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
 *   constraint_id: fraser_river_salmon_regulation
 *   human_readable: Fraser River Salmon Regulation
 *   domain: economic/environmental/indigenous_rights
 *
 * SUMMARY:
 *   The regulation of Fraser River salmon fishing emerged from genuine
 *   conservation concerns in the mid-20th century — salmon populations were
 *   under pressure from damming, pollution, and intensive harvest. However,
 *   the regulatory framework inherited the assumptions of settler
 *   colonialism: state sovereignty over natural resources, commercial
 *   property rights in licenses, and indigenous peoples as external claimants
 *   rather than primary stewards. Over five decades, the regulation has
 *   evolved into a stable hybrid that coordinates to prevent total collapse
 *   while extracting value from indigenous communities. The coordination
 *   function is real: without regulation, commercial interests would have
 *   driven populations to extinction through a race-to-the-bottom. But the
 *   extraction function is equally real: indigenous communities have been
 *   systematically restricted from harvesting that sustained their societies
 *   for millennia, while commercial operations expand under state-managed
 *   quotas. The increasing theater ratio (0.40 → 0.65) reflects expanding
 *   consultation, impact assessment, and conservation rhetoric that obscures
 *   unchanged power relationships. The rising extractiveness (0.35 → 0.58)
 *   reflects cumulative enclosure of indigenous harvesting rights and
 *   intensifying pressure on salmon populations from climate change, ocean
 *   conditions, and dam operations — factors the regulation cannot address
 *   because its underlying structure treats salmon as a resource to be
 *   exploited, not a species to be coexisted with.
 *
 * KEY AGENTS:
 *   - Indigenous First Nations Communities: Primary victims (powerless/trapped) — criminalizing subsistence harvesting, suppressing cultural practices, excluding from resource benefit
 *   - Commercial Fishing Industry: Primary beneficiaries (organized/arbitrage) — capture quota allocations, profit from state-managed sustainability, lobby for expanded harvest rights
 *   - Government Fisheries Management Authority: Institutional beneficiary (institutional/constrained) — generates license revenue, manages commercial sector, constrained by indigenous litigation and ecological evidence
 *   - Salmon Ecosystem: Subject of regulation, indexed as moderate/constrained — receives real protection from worst-case scenarios but experiences unsustainable extraction under current quota regime
 *   - Indigenous Rights and Environmental Movements: Organized challengers (organized/mobile) — building legal case, mobilizing international support, developing alternative governance models with sunset logic
 *   - Settler Colonial Property Rights Framework: Institutional substrate (institutional/arbitrage) — persists through inertia, performatively reformed through consultation but never redesigned
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fraser_river_salmon_regulation, 0.58).
domain_priors:suppression_score(fraser_river_salmon_regulation, 0.68).
domain_priors:theater_ratio(fraser_river_salmon_regulation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fraser_river_salmon_regulation, tangled_rope).
narrative_ontology:human_readable(fraser_river_salmon_regulation, "Fraser River Salmon Regulation").
narrative_ontology:topic_domain(fraser_river_salmon_regulation, "economic/environmental/indigenous_rights").

domain_priors:requires_active_enforcement(fraser_river_salmon_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fraser_river_salmon_regulation, commercial_fishing_industry).
narrative_ontology:constraint_beneficiary(fraser_river_salmon_regulation, government_revenue_agencies).
narrative_ontology:constraint_victim(fraser_river_salmon_regulation, indigenous_first_nations).
narrative_ontology:constraint_victim(fraser_river_salmon_regulation, salmon_population_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS FIRST NATIONS (SNARE) — Historically dependent on salmon for subsistence, cultural identity, and economic survival. Regulation has systematically restricted their catch rights while commercial operations expand. No exit option available — salmon are central to their territory and identity. Cannot migrate, renegotiate, or abandon reliance. Bears maximum extraction: traditional harvesting criminalized, cultural practices suppressed, resource wealth extracted by external actors.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COMMERCIAL FISHING INDUSTRY (ROPE) — Sees regulation as coordination mechanism solving collective action: catch limits prevent overfishing, quota systems allocate harvest rights, licensing creates market structure. Net beneficiary. Experiences regulation as enabling, not extractive. High arbitrage capacity — can relocate gear, shift to different species, lobby for advantageous quota allocations. Regulation subsidizes their business model by excluding indigenous competitors and capping total harvest below true scarcity point.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: GOVERNMENT FISHERIES AUTHORITY (TANGLED ROPE) — Mandated to protect salmon populations (genuine coordination function) but also generates license revenue and manages commercial quotas (asymmetric extraction). Constrained by legal framework and indigenous rights litigation. Benefits from licensing fees and commercial tax revenue; also bears institutional costs of treaty violations and regulatory complexity. Sees the constraint as hybrid: legitimate conservation tool plus enforcement mechanism for resource allocation favoring commercial over indigenous use.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SALMON ECOSYSTEM HEALTH (TANGLED ROPE) — Subject of the constraint, not an agent, but indexed as moderate/constrained because the regulation nominally protects salmon but actually enables extraction. Regulation provides real conservation benefits (prevents race-to-the-bottom overfishing) but also allows commercial extraction that degrades populations through dam operations, habitat loss, and climate stress. Salmon populations experience the constraint as both coordinating force (preventing extinction) and extraction mechanism (permitting unsustainable harvest). Constrained within the regulatory system with no autonomous exit.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INDIGENOUS RIGHTS AND ENVIRONMENTAL MOVEMENTS (SCAFFOLD) — Organized actors (indigenous coalitions, environmental NGOs, international bodies) view the regulation as a temporary institution being dismantled by legal and political pressure. Sunset mechanism: UN Declaration on the Rights of Indigenous Peoples, court rulings affirming consultation rights, and scientific evidence of ecosystem collapse are creating mandatory renegotiation points. Mobile capacity: movements can shift to litigation, international advocacy, direct action, and alternative governance models. Low effective extraction because the scaffold has explicit temporal limits and exit pathways are developing.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: SETTLER COLONIAL PROPERTY RIGHTS FRAMEWORK (PITON) — The underlying institutional structure (state sovereignty over natural resources, private property in commercial licenses) persists through inertia despite contradicting treaty obligations and indigenous inherent rights. The framework was never redesigned after contact; it was simply overlaid on existing indigenous governance. Regulation performs the ritual of 'scientific management' while maintaining underlying extraction relationships. Theatrical: impact assessments, consultation processes, and conservation narratives mask the fact that power allocation remains unchanged. Low effective extractiveness only because the framework's functional role (maintaining settler control) is degraded by litigation and indigenous assertion of authority.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective analyzing institutional resource flows, the Fraser River regulation is a classic tangled rope: it has a real coordination function (preventing salmon extinction) but it is structured to extract value from one group (indigenous nations) and transfer it to another (commercial capital + state revenue). The theatrical elements (consultation, impact assessments) make the extraction less visible but do not eliminate it. The analytical view captures that this is neither pure coordination nor pure extraction — it is a stable hybrid that serves both functions simultaneously.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fraser_river_salmon_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fraser_river_salmon_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fraser_river_salmon_regulation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fraser_river_salmon_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fraser_river_salmon_regulation, TR),
    TR >= 0.70.

:- end_tests(fraser_river_salmon_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regulation extracts value from indigenous communities through restriction of subsistence rights while enabling commercial extraction. However, it is not maximum extraction (0.70+) because the coordination function is genuinely necessary — without regulation, salmon populations would face extinction, harming everyone including commercial operators. The extractiveness increased from 0.35 to 0.58 over the interval as indigenous harvest rights were progressively restricted while commercial allocations expanded, and as ocean conditions and climate change created pressure requiring stronger regulatory control. Suppression (0.68): High. Indigenous communities face multiple barriers: legal prohibition on traditional harvesting, criminal penalties, licensing systems that exclude indigenous-scale operations, cultural suppression through school systems and religious institutions, institutional marginalization in decision-making, and lack of capital for commercial participation. These barriers are structural, not accidental. Theater ratio (0.65): Moderate-high. Consultation processes, impact assessments, scientific committees, and conservation rhetoric create performative legitimacy while masking unchanged power allocation. The theater increased from 0.40 to 0.65 as regulation became more sophisticated in its justifications without restructuring underlying extraction mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   Indigenous First Nations see a snare — they are trapped in a system where their historical, cultural, and subsistence rights are criminalized, while they have no exit option and no organizational capacity within the regulatory framework to protect themselves. Commercial fishing sees a rope — coordination preventing overfishing, market allocation via quotas, legitimate sustainable use. Government sees tangled rope — real conservation function but also real extraction of indigenous rights and environmental subsidy. Movements see a scaffold with sunset mechanism — recognizing this as a temporary institution that will be dismantled by litigation and international pressure. The piton perspective sees the underlying property rights framework itself as theatrical and degraded — persisting not because it functions but because alternative governance structures have not yet fully replaced it. These gaps are not measurement artifacts; they reflect genuinely different structural positions. An indigenous participant experiences maximum extraction; a commercial operator experiences coordination benefit; a government regulator experiences constrained management of mixed functions; a movement sees an institution scheduled for replacement.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial fishing industry: beneficiary with arbitrage exit (low d, negative chi). Can relocate operations, lobby for favorable quotas, participate in governance as equals, exit market if conditions deteriorate. Experiences regulation as enabling and coordinating rather than extractive. Indigenous communities: victims with trapped exit (high d, high chi). Cannot relocate salmon harvesting (salmon are place-based), cannot appeal to alternative governance (state has jurisdictional monopoly), cannot exit dependence (cultural and subsistence necessity). Experiences regulation as coercive extraction. Government authority: constrained institutional beneficiary (moderate d). Generates revenue from licenses, manages commercial sector, but also constrained by legal obligations and indigenous litigation. Benefits from the arrangement but cannot simply increase extraction without legal/political backlash. Movements and ecological interests: mobile organized actors (low d due to mobile/organized status). Can litigate, mobilize, shift to alternative governance models. Theater generates low experienced extraction because their exit pathways are developing. The piton perspective derives from the settler colonial property framework itself — it persists through inertia (institutional/arbitrage with low actual function), maintained by legal and cultural infrastructure that was never redesigned after contact.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids false reduction to pure extraction (snare everywhere) or pure coordination (rope everywhere) by capturing that BOTH functions are structurally real. The regulation genuinely prevents salmon extinction (coordination function) AND genuinely extracts value from indigenous communities (extraction function). These are not contradictory — the extraction is ENABLED BY the coordination mechanism. Commercial interests benefit from state-managed sustainability because it creates scarcity value for their licenses. Indigenous communities bear costs because the coordination is structured to exclude them and allocate benefits to commercial capital. The tangled rope classification captures this: the constraint cannot be eliminated without losing the coordination function, but it also cannot be reformed without addressing the extraction function. The scaffold perspective shows how the contradiction might be resolved: indigenous-led co-management with genuine jurisdiction would maintain salmon conservation while eliminating extraction. The piton perspective shows the persistence mechanism: the underlying settler colonial property framework is theatrical (consultation, impact assessment) but inert (power allocation unchanged) because no alternative governance structure has been fully established. The mandatrophy is resolved by recognizing that this constraint requires simultaneous restructuring of both the coordination mechanism (how salmon management happens) and the extraction mechanism (who benefits) — neither can be addressed independently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_food_security_threshold,
    'What harvest volume constitutes adequate indigenous food security and cultural practice, and how should this interact with commercial maximization?',
    'Community-based subsistence assessments; documentation of pre-contact harvest patterns; health and nutritional outcome tracking in communities under restrictive catch limits',
    'If threshold is met by current indigenous allocation: constraint is rebalanced as mixed extraction with reduced victim impact. If threshold remains unmet: extraction continues despite regulation''s conservation rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_food_security_threshold, empirical, 'Indigenous food security threshold under current regulation').

omega_variable(
    commercial_extraction_necessity,
    'How much of the commercial fishing extraction is structurally necessary for conservation coordination versus how much is contingent on current quota allocation?',
    'Population modeling showing maximum sustainable yield; comparison with indigenous-led management outcomes in regions with greater indigenous control; ecosystem recovery trajectories under different harvest scenarios',
    'If commercial extraction is necessary: tangled rope classification confirmed. If it is contingent: constraint could be restructured as rope with indigenous primary benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_extraction_necessity, empirical, 'Whether commercial extraction is structurally necessary or contingent').

omega_variable(
    treaty_obligation_enforceability,
    'Will courts and governments enforce indigenous treaty rights to meaningfully override current regulatory regime, or will enforcement remain symbolic?',
    'Tracking of court decisions, implementation of rulings, shifts in quota allocation, recognition of indigenous jurisdiction in fishery management',
    'If enforcement is meaningful: scaffold sunset mechanism activates within 10-20 years. If symbolic: piton classification becomes dominant and extraction persists through institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_obligation_enforceability, empirical, 'Enforceability of indigenous treaty rights against regulatory regime').

omega_variable(
    ecosystem_collapse_timeline,
    'What is the trajectory of salmon population decline under current regulation, and at what point does ecosystem failure force restructuring?',
    'Population genetics tracking, spawning ground surveys, dam removal outcomes, climate-driven migration pattern changes, returns projections',
    'If collapse is imminent (5-10 years): regulation loses coordination function and collapses into pure extraction (snare). If stable (50+ years): tangled rope persists. If recovering: classification shifts toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecosystem_collapse_timeline, empirical, 'Salmon population collapse timeline under current regulatory regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fraser_river_salmon_regulation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fsr_theater_1970, fraser_river_salmon_regulation, theater_ratio, 0, 0.4).
narrative_ontology:measurement(fsr_theater_1995, fraser_river_salmon_regulation, theater_ratio, 25, 0.55).
narrative_ontology:measurement(fsr_theater_2020, fraser_river_salmon_regulation, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(fsr_extractiveness_1970, fraser_river_salmon_regulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fsr_extractiveness_1995, fraser_river_salmon_regulation, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(fsr_extractiveness_2020, fraser_river_salmon_regulation, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fraser_river_salmon_regulation, resource_allocation).
narrative_ontology:affects_constraint(fraser_river_salmon_regulation, marine_resource_commons_extraction).
narrative_ontology:affects_constraint(fraser_river_salmon_regulation, indigenous_land_claim_suppression).
narrative_ontology:affects_constraint(fraser_river_salmon_regulation, commercial_fishery_quota_systems).

% DUAL FORMULATION NOTE:
% The Fraser River salmon regulation decomposes into three structurally distinct constraints: (1) Resource allocation coordination (preventing overfishing through quota systems, ε ≈ 0.15, primarily rope), (2) Indigenous rights suppression (systematic exclusion of indigenous harvest rights, ε ≈ 0.72, primarily snare), and (3) Commercial licensing extraction (state capturing value through quota allocation to commercial operators, ε ≈ 0.50, primarily tangled rope). This story addresses the hybrid constraint that emerges from their interaction. Individual constraint stories isolate each function; this story captures their systematic coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fraser_river_salmon_regulation, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
