% ============================================================================
% CONSTRAINT STORY: institutional_capacity_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_capacity_erosion, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_capacity_erosion
 *   human_readable: Institutional Capacity Erosion
 *   domain: institutional_governance/organizational_decline
 *
 * SUMMARY:
 *   Institutional capacity erosion is the progressive degradation of an
 *   organization's functional capability to deliver on its core mission,
 *   typically driven by budget constraints, staffing reductions, deferred
 *   maintenance, and technology debt accumulation. The constraint creates a
 *   structural tension between the institution's formal mandate (to deliver
 *   service, maintain legitimacy, coordinate collective action) and its
 *   diminishing material capacity to fulfill that mandate. This tension
 *   appears differently from different positions: frontline staff experience
 *   extraction; managers experience forced coordination under scarcity;
 *   budget actors experience manageable resource allocation; organized
 *   reformers see a temporary problem with recovery pathways; the institution
 *   itself develops a performative layer masking functional loss; and
 *   civilizational observers risk naturalizing institutional decline as
 *   inevitable. The constraint's extractiveness rises over time (0.32 → 0.58
 *   over 10 years) as budget pressure accumulates, while theater ratio also
 *   rises (0.38 → 0.68) as the institution increasingly performs legitimacy
 *   rather than delivering function. This dual rise is diagnostic: as
 *   extractiveness increases, theater must increase to maintain institutional
 *   legitimacy and prevent immediate collapse. The two metrics move together
 *   in capacity erosion because the same budget cuts that reduce function
 *   also eliminate the transparency and capacity-building that would expose
 *   the decline.
 *
 * KEY AGENTS:
 *   - Frontline Staff: Primary victim (powerless/trapped) — street-level workers absorbing impossible workloads, degraded tools, and career stagnation with no exit path
 *   - Mid-Level Managers: Secondary victim (moderate/constrained) — coordinate remaining function while experiencing budget pressure; can exit at significant career cost
 *   - Budget/Executive Actors: Primary beneficiary (institutional/arbitrage) — capture efficiencies, advance through cost-cutting success, maintain career mobility
 *   - Institutional Mission Function: Primary victim (abstract) — the organization's ability to coordinate and deliver on original mandate degrades systematically
 *   - Reform Coalition: Organized agents (organized/mobile) — civil society, unions, external monitors with leverage to demand restructuring or recovery
 *   - Institutional Legitimacy Layer: Institutional actor (institutional/arbitrage) — the performance of compliance, accountability, and standard operations continues even as function atrophies
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as structural inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_capacity_erosion, 0.58).
domain_priors:suppression_score(institutional_capacity_erosion, 0.65).
domain_priors:theater_ratio(institutional_capacity_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_capacity_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_capacity_erosion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_capacity_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_capacity_erosion, tangled_rope).
narrative_ontology:human_readable(institutional_capacity_erosion, "Institutional Capacity Erosion").
narrative_ontology:topic_domain(institutional_capacity_erosion, "institutional_governance/organizational_decline").

domain_priors:requires_active_enforcement(institutional_capacity_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_capacity_erosion, cost_reduction_agents).
narrative_ontology:constraint_beneficiary(institutional_capacity_erosion, short_term_extractors).
narrative_ontology:constraint_victim(institutional_capacity_erosion, institutional_legitimacy).
narrative_ontology:constraint_victim(institutional_capacity_erosion, long_term_mission_fidelity).
narrative_ontology:constraint_victim(institutional_capacity_erosion, frontline_staff).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE STAFF (SNARE) — Street-level workers (teachers, caseworkers, inspectors, maintenance crews) are trapped in declining institutions with no exit options. They bear the full cost of capacity erosion through impossible workloads, degraded tools, mounting bureaucratic overhead masking resource scarcity, and career stagnation. No alternative employment paths in the same mission domain. Institutional decline extracts from them maximally.
constraint_indexing:constraint_classification(institutional_capacity_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL MANAGERS (TANGLED ROPE) — Face genuine coordination demands (allocating scarce resources, maintaining institutional function with degraded capacity) while also experiencing asymmetric extraction. High career switching costs if they leave to private sector. Can exit at significant cost (career interruption, relocation, retraining). The constraint enforces genuine coordination of remaining function alongside extractive budget pressure.
constraint_indexing:constraint_classification(institutional_capacity_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUDGET CONSOLIDATION ACTORS (ROPE) — Treasury departments, central administrations, and executive leadership experience the constraint as coordination: managing resource scarcity, balancing competing institutional demands, consolidating functions. They have arbitrage options (rotate to other agencies or private consulting). Net beneficiary through career advancement opportunities and reduced accountability for institutional decline (blame flows downward). Experience the constraint as manageable coordination problem.
constraint_indexing:constraint_classification(institutional_capacity_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized civil society actors (unions, oversight bodies, external auditors, advocacy groups) with mobile exit options see capacity erosion as a temporary governance failure with potential sunset. Reform mechanisms (digital-first modernization, outsourcing, restructuring, reallocation) promise recovery. Low effective extraction because organized agents see intervention pathways and possess external leverage. Sunset logic applies if reforms are genuinely implemented.
constraint_indexing:constraint_classification(institutional_capacity_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL LEGITIMACY THEATER (PITON) — Institutions maintain performative compliance (annual reports showing metrics, public accountability forums, standard operating procedures) while actual capacity to deliver has atrophied. The constraint persists through institutional inertia — institutions continue performing legitimacy rituals (compliance audits, planning cycles, stakeholder consultations) that mask rather than address functional erosion. Theater ratio rises as function declines. The institution has 'become' its own performance rather than its original mission.
constraint_indexing:constraint_classification(institutional_capacity_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL VIEW (MOUNTAIN) — From a civilizational/universal perspective, capacity erosion may appear as a structural law: institutions inevitably decline when funding pressures accumulate, short-term extractors outcompete long-term mission advocates, and political attention fragments. This perspective naturalizes the erosion as inevitable bureaucratic decay. However, this classification is vulnerable to false summit detection — the structural data reveals contingent institutional choices (budget allocation, staffing decisions, outsourcing policies) rather than immutable laws. Mountain classification here represents naturalization risk.
constraint_indexing:constraint_classification(institutional_capacity_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_capacity_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_capacity_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_capacity_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_capacity_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_capacity_erosion, TR),
    TR >= 0.70.

:- end_tests(institutional_capacity_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from institutional mission fidelity and frontline capacity, but the extraction is not absolute — the institution continues to function partially. Much of the 'extraction' is medium-term: budget cuts generate short-term savings at long-term mission cost. The value reflects that this is a mixed coordination-extraction dynamic rather than pure extraction. Suppression (0.65): Moderate-high. Significant barriers prevent exit and recovery: frontline workers lack alternative employment in mission domains; mid-managers face career switching costs; reform requires political support and resource investment; institutional actors have vested interests in current structures. But suppression is not total — some institutions do recover, and reform coalitions maintain pressure. Theater ratio (0.68): High and rising. As capacity erodes, the institution's formal performance (compliance reports, stakeholder consultations, strategic plans, audit responses) becomes increasingly disconnected from actual delivery capability. The institution invests in visibility (annual reports showing 'efficiency gains') while functionality declines. Theater rises because it's cheaper than actual capacity building and maintains the fiction of institutional competence. The measurement arc (0.38 → 0.68 over 10 years) shows this acceleration: early erosion can be masked with modest theater; deep erosion requires elaborate performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a four-way perspectival split that reveals hidden structural complexity. Budget actors see Rope (manageable resource coordination). Frontline staff see Snare (maximum extraction with no exit). Managers see Tangled Rope (mixed coordination and extraction). Reformers see Scaffold (temporary problem with sunset pathway). The institution itself sees Piton (performing legitimacy while function atrophies). The analytical observer risks seeing Mountain (institutional decline as inevitable structural law) but structural data contradicts this — the apparent 'inevitability' is contingent on budget allocation choices, staffing decisions, and political attention. The perspectival gap between beneficiary and victim is stark: the same budget cuts that are experienced as manageable resource allocation at the executive level are experienced as impossible extraction at the frontline. This gap is diagnostic of extractive asymmetry masked as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary-victim structure: Budget consolidation actors (treasury, executive leadership) are the net beneficiaries — they capture cost-cutting achievements, advance career through 'efficiency,' and maintain arbitrage exit options to other agencies or private roles. The institutional mission (delivery capability) and frontline staff are the victims — they bear the cost of budget cuts through degraded service and impossible working conditions. Mid-level managers are caught in between: they must coordinate the institution's remaining function (a genuine coordination demand) while also experiencing extraction pressure (budget targets, workload intensification, career stagnation). The reform coalition benefits from the constraint's visibility but is not directly victimized — they have external leverage and mobile exit. The institutional legitimacy layer (compliance theater) benefits passively from cost constraints — when actual function is expensive and budget is tight, performance becomes the default survival strategy. Directionality values: beneficiaries with arbitrage exit show low d (0.15–0.25); trapped frontline shows high d (0.85–0.95); constrained managers show moderate d (0.60–0.70); organized reformers show low-moderate d (0.35–0.45) because they have leverage even without internal exit.
 *
 * MANDATROPHY ANALYSIS:
 *   Capacity erosion presents a mandatrophy case that requires decomposition. The constraint as stated conflates two structurally distinct claims: (1) institutional function inevitably declines when budget pressure accumulates — this appears as Mountain from some perspectives; (2) the extraction of frontline capacity and mission fidelity is a deliberate or systemic institutional choice that benefits certain actors — this appears as Snare/Tangled Rope from victim perspectives. The mandatrophy is resolved by recognizing these are not different views of one constraint but different constraints in a family. The structural constraint (budget pressure → capacity loss) has genuine Mountain properties in some domains (physical laws of maintenance, scaling costs of specialization). The extractive constraint (budget cuts systematized to benefit executives while harming mission) is genuinely extractive asymmetry. The institution's performance of legitimacy (Piton) is a separate phenomenon arising from the extraction-function gap. Future analysis should create three linked stories: institutional_functional_decline (Mountain), capacity_extraction_asymmetry (Tangled Rope/Snare), and institutional_legitimacy_theater (Piton). Current story subsumes all three, creating mandatrophy. The Tangled Rope classification is appropriate for the composite, but the commentary must note that the 'mountain-ness' of the structure (the appearance of inevitability) is a false summit created by naturalization of extractive institutional choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_systemic_erosion,
    'Is capacity erosion a deliberate extraction strategy (privatization agenda, union-busting, political defunding) or a systemic consequence of competing budget pressures and incentive misalignment?',
    'Comparative institutional analysis: budget-cutting patterns across jurisdictions; correlation between erosion rates and political leadership ideology; evidence of planned privatization or alternative service delivery',
    'If intentional: classification shifts toward Snare (deliberate extraction mechanism). If systemic: remains Tangled Rope (coordination failure embedded in extraction). If mixed: dual constraint stories needed for strategic vs structural dimensions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_vs_systemic_erosion, empirical, 'Whether capacity erosion is intentional strategy or systemic byproduct').

omega_variable(
    recovery_pathway_viability,
    'Can eroded institutions recover functional capacity through reform without structural dissolution or privatization, or is capacity loss below a critical threshold beyond recovery?',
    'Historical case studies of institutional recovery; modeling of minimum viable staffing/resource levels; evidence from institutions that have successfully reversed erosion trajectories',
    'If recovery possible: Scaffold sunset logic is credible. If recovery impossible: classification shifts toward Snare (permanent extraction) or Piton (institutional zombification). If threshold-dependent: multiple stories at different erosion stages.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recovery_pathway_viability, empirical, 'Whether institutional capacity erosion is reversible through reform').

omega_variable(
    extraction_beneficiary_diffusion,
    'Who specifically benefits from institutional capacity erosion? Tax reduction beneficiaries, privatization bidders, political actors seeking to delegitimize public institutions, or systemic rent-extractors?',
    'Beneficiary tracing analysis: which constituencies benefit from reduced institutional spending; whose labor capacity increases as public institution capacity decreases; evidence of wealth transfer from public to private sectors',
    'If concentrated beneficiary (privatizers, tax cutters): Snare classification more accurate. If diffuse beneficiary (general taxpayers, political coalitions): true extraction less certain. If negative-sum (no one benefits, all lose): reframe as coordination failure rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_diffusion, empirical, 'Identification and diffusion of beneficiaries from capacity erosion').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of exit/recovery primarily structural (economic barriers, legal constraints) or internalized (institutional actors have accepted erosion as inevitable, normalized decline as natural)?',
    'Survey data on institutional actor beliefs about changeability; analysis of reform proposal adoption rates vs feasibility; evidence of psychological resignation vs material barriers',
    'If structural: address barriers to change; suppression persists post-exit. If internalized: identity-lock dynamics dominate; exit requires cognitive reframing, not just resource reallocation. If mixed: require dual intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized suppression of recovery').

omega_variable(
    temporal_boundary_ambiguity,
    'At what point does an eroding institution become permanently zombified? Is there a clear transition from degraded-but-functional to functionally extinct?',
    'Longitudinal institutional performance data; identification of failure cascade thresholds; case studies of institutional collapse points',
    'If clear threshold: Piton classification appropriate at pre-threshold, different type (Snare or defunct) post-threshold. If gradual continuum: single Tangled Rope/Piton framework may misclassify transition dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_boundary_ambiguity, conceptual, 'Definition of zombie institutional threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_capacity_erosion, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icap_tr_t0, institutional_capacity_erosion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(icap_tr_t5, institutional_capacity_erosion, theater_ratio, 5, 0.52).
narrative_ontology:measurement(icap_tr_t10, institutional_capacity_erosion, theater_ratio, 10, 0.68).
narrative_ontology:measurement(icap_tr_t15, institutional_capacity_erosion, theater_ratio, 15, 0.79).

% Extraction over time
narrative_ontology:measurement(icap_be_t0, institutional_capacity_erosion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(icap_be_t5, institutional_capacity_erosion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(icap_be_t10, institutional_capacity_erosion, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(icap_be_t15, institutional_capacity_erosion, base_extractiveness, 15, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_capacity_erosion, resource_allocation).
narrative_ontology:boltzmann_floor_override(institutional_capacity_erosion, 0.18).
narrative_ontology:affects_constraint(institutional_capacity_erosion, regulatory_capture_institutional_level).
narrative_ontology:affects_constraint(institutional_capacity_erosion, public_private_partnership_extraction).
narrative_ontology:affects_constraint(institutional_capacity_erosion, budget_politics_accumulation).

% DUAL FORMULATION NOTE:
% Institutional capacity erosion is downstream of budget allocation decisions and political attention deficits, which are themselves constraints with their own extractiveness. The constraint decomposes into functional decline (Mountain-like), extractive asymmetry (Snare/Tangled Rope), and legitimacy performance (Piton). Current story models the composite; future family should separate these three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_capacity_erosion, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
