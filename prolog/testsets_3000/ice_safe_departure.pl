% ============================================================================
% CONSTRAINT STORY: ice_safe_departure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ice_safe_departure, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ice_safe_departure
 *   human_readable: ICE Safe Departure Program
 *   domain: political/immigration_enforcement
 *
 * SUMMARY:
 *   The ICE Safe Departure Program represents a structural tension between
 *   humanitarian procedure and enforcement extraction. Officially, it allows
 *   immigrants with final deportation orders to depart voluntarily without
 *   detention, criminal record complications, or asset seizure during
 *   removal. In practice, the program operates within a coercive framework:
 *   final deportation order means all legal appeals are exhausted, and the
 *   choice between 'safe departure' and 'detention-based removal' is not a
 *   choice between staying and leaving but between modes of forced removal.
 *   The constraint exhibits Tangled Rope structure: it offers genuine
 *   coordination benefit (avoids detention costs for both ICE and deportees,
 *   reduces operational friction) while extracting through family separation,
 *   status loss, and diaspora effects. The program's theater ratio has risen
 *   as it has become the primary legitimacy narrative for ICE enforcement —
 *   appearing humanitarian while executing removal. The extractiveness score
 *   (0.52) reflects that extraction is moderate relative to raw
 *   detention-based enforcement but substantial in absolute terms (family
 *   permanence, community loss, documented status erasure).
 *
 * KEY AGENTS:
 *   - ICE Enforcement Apparatus: Primary beneficiary (institutional/arbitrage) — reduces detention costs, operational friction, and reputational damage while maintaining removal authority
 *   - Deportable Immigrants Without Legal Resources: Primary victim (powerless/trapped) — forced choice between departure and detention; no legal recourse or alternative paths
 *   - Deportable Immigrants With Legal Advocacy: Secondary victim with agency (moderate/constrained) — can negotiate terms through legal aid; still experience family separation and status loss
 *   - Immigrant Advocacy Coalition: Organized intermediary (organized/constrained) — negotiates program terms and mitigation; sees potential sunset as enforcement environment shifts
 *   - Deported Communities and Diaspora Effects: Aggregate victim (moderate/constrained) — experience economic loss, caregiving gap, remittance reduction, family separation
 *   - Voluntary Departure Institutional Ritual: Performative structure (institutional/arbitrage) — maintains legitimacy theater; function has atrophied to pure narrative maintenance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees hybrid extraction and coordination; identifies contingency of removal policy versus naturalization as necessary enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ice_safe_departure, 0.52).
domain_priors:suppression_score(ice_safe_departure, 0.68).
domain_priors:theater_ratio(ice_safe_departure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_safe_departure, extractiveness, 0.52).
narrative_ontology:constraint_metric(ice_safe_departure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ice_safe_departure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ice_safe_departure, tangled_rope).
narrative_ontology:human_readable(ice_safe_departure, "ICE Safe Departure Program").
narrative_ontology:topic_domain(ice_safe_departure, "political/immigration_enforcement").

domain_priors:requires_active_enforcement(ice_safe_departure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ice_safe_departure, ice_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(ice_safe_departure, voluntary_departure_participants).
narrative_ontology:constraint_victim(ice_safe_departure, deported_individuals).
narrative_ontology:constraint_victim(ice_safe_departure, family_separation_externality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPORTABLE IMMIGRANT WITHOUT LEGAL RESOURCES (SNARE) — Final deportation order issued; lacks legal recourse or asylum claims. 'Safe departure' is coercive: accept voluntary departure to avoid detention conditions, criminal record complications, and asset seizure during removal. No meaningful choice between deportation and departure; both extract through separation from family, livelihood, and documented legal status. Trapped exit with maximum suppression.
constraint_indexing:constraint_classification(ice_safe_departure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEPORTABLE IMMIGRANT WITH LEGAL ADVOCACY (TANGLED ROPE) — Legal aid organizations, community networks, and immigrant advocacy groups can negotiate terms: timing of departure, asset access, family communication. The program offers coordination benefit (avoids detention during appeals processing, preserves some agency in departure timing) alongside extraction (family separation, loss of documented status, community removal). Active enforcement required from ICE (deportation threat). Constrained exit with moderate suppression.
constraint_indexing:constraint_classification(ice_safe_departure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ICE ENFORCEMENT APPARATUS (ROPE) — Primary beneficiary. Safe departure enables case closure without detention costs, courtroom processing, or in-custody mortality risk. Solves coordination problem: reduces ICE resource burden while appearing humane. Voluntary departure reduces operational friction and administrative overhead. Arbitrage exit with high benefit flow.
constraint_indexing:constraint_classification(ice_safe_departure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMMIGRANT ADVOCACY COALITION (SCAFFOLD) — Organized advocacy groups (American Civil Liberties Union, Immigration Legal Resource Center, Faith in Action) negotiate within the program: extended departure windows, asset liquidation periods, family communication access. Program functions as temporary coordination mechanism with implicit sunset: as enforcement environments shift, immigration reform proceeds, or due process protections strengthen, the program's enforcement theater declines. Suppression is high but structured to decline over generational horizon.
constraint_indexing:constraint_classification(ice_safe_departure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VOLUNTARY DEPARTURE INSTITUTIONAL RITUAL (PITON) — The program's central claim is that departure is 'voluntary' and 'safe.' This ritual is substantially performative: given a final deportation order, the choice is absence of coercion during removal (via voluntary departure) versus presence of coercion (detention, criminal charges). Neither option is genuinely chosen; the ritual's function has atrophied — it no longer coordinates anything substantive but persists as a due-process theater maintaining legitimacy appearance. Theater ratio high (0.58+); extractiveness low relative to raw coercion; maintained through institutional inertia.
constraint_indexing:constraint_classification(ice_safe_departure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DEPORTED COMMUNITIES AND DIASPORA EFFECTS (SNARE) — The aggregate effect of safe departure removes social capital, labor, remittances, and caregiving from origin communities. Families experience separation trauma with limited legal recourse. Communities lose human resources while bearing diaspora integration costs. This perspective aggregates across many individuals but treats removal as extractive from collective capacity. Constrained exit (immigration restrictions prevent return); significant suppression (legal barriers to family reunification, employment authorization in destination countries).
constraint_indexing:constraint_classification(ice_safe_departure, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global view, safe departure is a genuine hybrid: it offers some coordination benefit (avoids detention violence, maintains individual agency in departure timing) while extracting through family separation, status loss, and community removal. The program structures removal with less coercive theater than detention-based enforcement, but the underlying extraction (deportation itself) remains intact. Not a mountain (removal is contingent policy, not natural law). Tangled rope with moderate extractiveness and high suppression.
constraint_indexing:constraint_classification(ice_safe_departure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_safe_departure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ice_safe_departure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ice_safe_departure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ice_safe_departure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ice_safe_departure, TR),
    TR >= 0.70.

:- end_tests(ice_safe_departure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The program extracts through forced removal, family separation, and status loss, but offers some individual agency (departure timing, asset access through legal negotiation) not available in detention-based enforcement. The extraction is not maximal because the program genuinely reduces coercive theater relative to detention alternatives. However, the underlying removal authority (deportation itself) remains intact and non-negotiable. The rising extractiveness trajectory (0.38→0.52 over interval) reflects that as the program became the dominant enforcement narrative, it absorbed more extraction through its legitimacy function. Suppression (0.68): High. Barriers include final deportation order (no legal recourse), family separation (emotional/relational suppression), documentation loss (economic suppression), limited destination country employment authorization (future suppression), visa backlog for family reunification (institutional suppression). Theater ratio (0.58): Moderate-high. The program's central claim of 'voluntariness' and 'safety' is substantially theatrical — given a final deportation order, neither voluntary departure nor detention is genuinely chosen. The language of 'safety' obscures the coercive framework. Theater has increased as the program has become the primary humanitarian narrative for enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The gap between ICE's Rope classification and the powerless deportable immigrant's Snare classification is maximal. ICE genuinely experiences the program as solving a coordination problem: voluntary departure reduces detention costs, operational friction, and reputational exposure while maintaining removal authority. This is legitimate coordination benefit. The powerless deportable immigrant genuinely experiences coercive removal with theater of choice — the final deportation order eliminates legal agency, and 'safe departure' is selection between modes of forced removal. Both perspectives are structurally accurate descriptions of the same constraint. The gap reveals that 'safe departure' is a genuine coordination mechanism for enforcement capacity alongside a genuine extraction mechanism for individual agency. The moderate immigrant with legal advocacy occupies an intermediate position: they can negotiate timing, asset access, and communication, reducing experienced extraction relative to the powerless case, but they still bear family separation and status loss. The analytical observer's Tangled Rope classification resolves the gap: the program is both coordination (benefits ICE, reduces detention costs) and extraction (removes individuals, separates families, erases documented status). The classification system prevents naturalizing either the coordination or the extraction as a necessary feature of immigration enforcement versus a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   ICE derives low d from beneficiary status (extraction flows toward this agent) and arbitrage exit (can exit the constraint by choosing alternative enforcement). Beneficiary → low d; arbitrage → lower d. The sigmoid f(d) produces negative or minimal χ: ICE experiences the constraint as beneficial. Deportable immigrants derive high d from victim status (extraction flows away from this agent) and trapped exit (cannot exit removal authority). Victim → high d; trapped → maximum d. The sigmoid f(d) produces high χ: deportable immigrants experience maximum extraction relative to their power. Legal advocacy organizations derive moderate d from victim-adjacent status (they represent victims but are not themselves deported) and constrained exit (can negotiate but cannot reverse deportation). The piton classification derives from the theater gate: the voluntary departure ritual is substantially performative (theater_ratio 0.58) while maintaining low functional extraction for the ritual itself — the extraction happens via the underlying deportation authority, not via the voluntary departure mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The program avoids mandatrophy by correctly identifying itself as Tangled Rope rather than misclassifying as pure Rope (coordination only) or pure Snare (extraction only). The beneficiary (ICE) experiences genuine coordination benefit — the program solves the operational problem of case closure without detention costs or reputational damage. The victim (deportable immigrant) experiences genuine extraction — removal, family separation, status loss. The program requires active enforcement (ICE authority, deportation orders) and offers coordination function (reduced detention costs). Both gatekeeping conditions for Tangled Rope are met. The false summit risk would be classifying the program as a Rope (pure coordination) based on the ICE perspective alone, naturalizing the extraction as an inherent feature of immigration policy rather than a contingent institutional choice. The piton classification of the 'voluntary departure ritual' correctly identifies that the humanitarianism language is performative — the underlying extraction (deportation) is real, but the ritual's function has become narrative maintenance rather than substantive improvement in deportee experience relative to what legal obligation requires. The mandatrophy is resolved by refusing to collapse the perspectival gap: the program is genuinely hybrid, with different agents experiencing it as coordination or extraction depending on their structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_departure_voluntariness,
    'Is departure truly voluntary when the alternative is detention with criminal record complications?',
    'Comparative analysis of outcomes: voluntary departure vs. detention-based removal. Survey data on deportee perception of choice. Legal analysis of coercion threshold in immigration law.',
    'If voluntariness threshold is met: classification shifts toward Rope (coordination with choice). If voluntariness fails: classification remains Snare (coercive removal with theater of choice). Current impact: heightened suppression score (0.68).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_departure_voluntariness, conceptual, 'Whether safe departure constitutes genuine voluntary choice or coerced selection').

omega_variable(
    family_separation_permanence,
    'What fraction of deported individuals successfully reunify with separated family members within 5 years?',
    'Longitudinal tracking of deportees post-removal. Survey data on family contact and reunification attempts. Legal barrier analysis (immigration backlog timelines, visa availability).',
    'If reunification rate > 40%: extraction is temporary/reversible (scaffold logic). If reunification rate < 10%: extraction is permanent diaspora effect (snare logic). Current impact: victims classification assumes low reunification, high permanence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_separation_permanence, empirical, 'Permanence of family separation caused by safe departure').

omega_variable(
    ice_resource_substitution,
    'Does safe departure reduce total ICE enforcement resource expenditure, or does it redirect resources to other enforcement mechanisms?',
    'Budget analysis: detention costs vs. voluntary departure processing vs. enforcement expansion. Temporal analysis of enforcement intensity pre/post safe departure program introduction.',
    'If net resource reduction: program provides genuine coordination benefit (Rope logic strengthened). If resources redirect to other enforcement: no net coordination benefit; pure extraction with reduced theater (Snare logic strengthened, theater ratio may decline).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ice_resource_substitution, empirical, 'Whether safe departure reduces or redirects ICE enforcement resources').

omega_variable(
    due_process_theater_effectiveness,
    'Does the voluntary departure offer increase perceived legitimacy of deportation enforcement, thereby reducing legal/political challenges?',
    'Analysis of legal challenge rates pre/post program. Survey data on public perception of ICE enforcement legitimacy. Media frame analysis (coercion vs. humanitarian language).',
    'If legitimacy increases substantially: theater is functionally strengthening extraction through reduced opposition (Piton with high extraction). If legitimacy gains are minimal: theater is truly performative (Piton with low extraction). Current impact: theater_ratio reflects moderate theater (0.58).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(due_process_theater_effectiveness, empirical, 'Whether voluntary departure theater increases enforcement legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ice_safe_departure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ice_sd_tr_t0, ice_safe_departure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ice_sd_tr_t5, ice_safe_departure, theater_ratio, 5, 0.5).
narrative_ontology:measurement(ice_sd_tr_t10, ice_safe_departure, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(ice_sd_be_t0, ice_safe_departure, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ice_sd_be_t5, ice_safe_departure, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(ice_sd_be_t10, ice_safe_departure, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ice_safe_departure, enforcement_mechanism).
narrative_ontology:affects_constraint(ice_safe_departure, detention_asylum_barriers).
narrative_ontology:affects_constraint(ice_safe_departure, family_separation_chain_migration).
narrative_ontology:affects_constraint(ice_safe_departure, documented_status_employment_access).

% DUAL FORMULATION NOTE:
% Safe Departure operates at the enforcement coordination level (reducing detention costs) but has upstream effects on family separation and documented status access. The program is downstream of broader deportation authority (final orders issued) but structures the removal mechanism itself. Network links capture how changes to Safe Departure (expansion, restriction, or elimination) would propagate to family separation extraction and documented status barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
