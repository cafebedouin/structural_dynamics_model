% ============================================================================
% CONSTRAINT STORY: appropriations_brinkmanship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_appropriations_brinkmanship, []).

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
 *   constraint_id: appropriations_brinkmanship
 *   human_readable: Government Shutdown Threat via Appropriations Process
 *   domain: political/legislative
 *
 * SUMMARY:
 *   The appropriations brinkmanship constraint describes the recurring
 *   political tactic of using the legislative appropriations deadline as a
 *   coercive leverage point to force policy concessions. The mechanism
 *   exploits a constitutional design vulnerability: the requirement for
 *   regular appropriations bills creates an annual deadline at which
 *   government funding lapses, forcing all factions to negotiate. One faction
 *   (typically the legislative majority or an organized minority bloc)
 *   threatens shutdown to extract policy concessions unrelated to budget
 *   disagreement itself. The constraint operates through collective coercion:
 *   federal workers face wage withholding, benefit recipients face service
 *   disruption, and the dispersed public bears the costs of service failure.
 *   The extraction flows from political actors (beneficiaries) through
 *   threatened shutdown to vulnerable populations (victims). Over the past
 *   decade, the theater_ratio has risen from 0.35 to 0.65 as the executive
 *   branch developed administrative workarounds — contingency planning,
 *   carryover authority, and emergency powering of essential functions have
 *   reduced actual government halt, making the mechanism increasingly
 *   performative while preserving its coercive force through the threat
 *   alone.
 *
 * KEY AGENTS:
 *   - Legislative Majority Faction: Primary beneficiary (institutional/arbitrage) — uses shutdown threat as leverage to impose policy; experiences constraint as negotiating tool with low personal cost
 *   - Federal Workers: Primary victim (powerless/trapped) — face immediate wage withholding and service disruption; cannot exit federal employment quickly
 *   - Benefit Recipients (Social Security, Medicare, SNAP): Primary victim (powerless/trapped) — face payment delays and service loss; trapped by dependency on federal benefits
 *   - General Public Dependent on Federal Services: Distributed victim (powerless/trapped) — depend on services that shut down (airport security, national parks, disaster response) with no exit option
 *   - Legislative Minority Faction (if organized as veto bloc): Secondary victim (organized/constrained) — uses shutdown threat as coercive leverage but faces organizational and electoral constraints
 *   - Executive Branch and Administrative State: Tertiary actor (institutional/arbitrage) — mitigates shutdown severity through administrative continuity, reducing actual harm and increasing theater ratio
 *   - Media and Public Attention System: Beneficiary (moderate/mobile) — captures attention and narrative framing from shutdown cycles; benefits from political theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(appropriations_brinkmanship, 0.58).
domain_priors:suppression_score(appropriations_brinkmanship, 0.72).
domain_priors:theater_ratio(appropriations_brinkmanship, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(appropriations_brinkmanship, extractiveness, 0.58).
narrative_ontology:constraint_metric(appropriations_brinkmanship, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(appropriations_brinkmanship, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(appropriations_brinkmanship, snare).
narrative_ontology:human_readable(appropriations_brinkmanship, "Government Shutdown Threat via Appropriations Process").
narrative_ontology:topic_domain(appropriations_brinkmanship, "political/legislative").

domain_priors:requires_active_enforcement(appropriations_brinkmanship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(appropriations_brinkmanship, legislative_majority_faction).
narrative_ontology:constraint_victim(appropriations_brinkmanship, federal_workers).
narrative_ontology:constraint_victim(appropriations_brinkmanship, benefit_recipients).
narrative_ontology:constraint_victim(appropriations_brinkmanship, public_services).
narrative_ontology:constraint_victim(appropriations_brinkmanship, minority_faction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL WORKERS AND BENEFIT RECIPIENTS (SNARE) — Cannot exit the appropriations cycle; face immediate wage withholding, benefit delays, and service loss if shutdown occurs. Zero exit options. Maximum extraction through coercion — the constraint's mechanism directly targets survival-level costs. No viable alternative pathways exist for individuals dependent on federal employment or benefits.
constraint_indexing:constraint_classification(appropriations_brinkmanship, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC DEPENDENT ON FEDERAL SERVICES (SNARE) — Cannot exit or bypass federal services (Social Security, Medicare, food assistance, disaster response, airport security, national parks). The shutdown mechanism uses collective vulnerability as leverage. Extraction flows from political actors through service disruption to dispersed vulnerable populations. High suppression: no organized advocacy structure represents the diffuse public interest against appropriations brinkmanship.
constraint_indexing:constraint_classification(appropriations_brinkmanship, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE MINORITY FACTION (TANGLED ROPE) — Has veto power through filibuster or voting bloc but must extract concessions through shutdown threat rather than genuine negotiation. Organized but constrained: party discipline and electoral concerns limit exit options. Experiences both coordination (legitimate minority protection) and extraction (coercing majority through public harm). Asymmetric leverage mechanism violates normal legislative process.
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE MAJORITY FACTION (ROPE) — Benefits from the appropriations mechanism as a coordination device for imposing policy. Experiences the constraint as negotiation leverage — can withhold appropriations to force concessions. Arbitrage exit: can always capitulate or compromise without existential cost. The majority perceives the threat as a tool, not a trap.
constraint_indexing:constraint_classification(appropriations_brinkmanship, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE BRANCH AND ADMINISTRATIVE STATE (PITON) — Maintains contingency plans, carryover account reserves, and emergency authority procedures that mitigate actual shutdown severity. Sees the threat as theater: the executive has enough structural autonomy to soften impacts and continue essential functions. The shutdown mechanism is largely performative — actual government halt is prevented by administrative improvisation. Theater ratio increases because both legislative factions know the executive will find workarounds. Inertial: the appropriations cycle persists despite reduced functional force.
constraint_indexing:constraint_classification(appropriations_brinkmanship, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — CONSTITUTIONAL VULNERABILITY (TANGLED ROPE) — From a civilizational view, the appropriations mechanism reveals a genuine constitutional design problem: no framers envisioned that regular appropriations would become a hostage-taking mechanism. The constraint exhibits both coordination (legitimate budget authority) and extraction (coercion through collective harm). The analytical perspective sees the tension between the constitutional division of powers and the political weaponization of budgetary deadlines. This is not an immutable constitutional law but a design vulnerability that some constitutional systems (parliamentary democracies with confidence votes) have resolved differently.
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(appropriations_brinkmanship_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(appropriations_brinkmanship, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(appropriations_brinkmanship, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(appropriations_brinkmanship, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(appropriations_brinkmanship, TR),
    TR >= 0.70.

:- end_tests(appropriations_brinkmanship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant policy concessions from legislative minorities and the public through the coercive threat of service disruption. The extraction is not maximum (≥0.66) because the executive branch's administrative workarounds reduce actual harm, limiting the threat's real coercive force. Suppression (0.72): High. Multiple barriers prevent escape: federal workers cannot instantly find new employment; benefit recipients cannot opt out of dependency; the general public has no bypass for federal services; legislative minorities face party discipline and electoral consequences for capitulation. No organized advocacy structure represents the dispersed victims against appropriations brinkmanship. Theater ratio (0.65): Moderate-high and rising. Early shutdowns (2013, 2018) caused measurable service disruptions and economic harm. Recent shutdowns (2021-2023) have been progressively shorter and less damaging due to improved executive contingency planning. The mechanism increasingly operates through threat rather than actual harm — both sides know the executive will preserve essential functions, making the ritual increasingly performative while suppression remains high. The rising theater ratio over the interval reflects Goodhart drift: as players learn to mitigate actual impacts, the constraint's function shifts from coordination failure (we genuinely cannot agree on budget) to extraction mechanism (we use the threat to coerce policy).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between beneficiaries and victims. The legislative majority sees Rope — a legitimate coordination mechanism for imposing policy preferences through budgetary authority. Federal workers see Snare — pure coercion with no exit. The executive branch sees Piton — the threat is theater because administrative continuity mitigates harm. The legislative minority sees Tangled Rope — they have veto power (genuine coordination) but must use it coercively (extraction). The dispersed public sees Snare — they cannot organize or exit and bear diffuse collective costs. The analytical observer sees Tangled Rope — the constraint embeds both legitimate constitutional division of powers (coordination) and design vulnerability (extraction). The gap exists because the constraint's extraction mechanism operates through collective harm that is invisible to direct beneficiaries — the majority faction experiences only the policy leverage, not the wage withholding experienced by federal workers.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal workers and benefit recipients experience maximum directionality (d ≈ 0.95) — they are pure victims with trapped exit options and no structural benefits from the appropriations mechanism. They bear full extraction: wage withholding and service loss. The general public dependent on federal services is similarly high-directionality (d ≈ 0.90) but slightly more diffuse, making collective action harder and suppression higher. The legislative majority faction experiences low directionality (d ≈ 0.15) — they are beneficiaries with arbitrage exit options who can always capitulate without existential cost. The minority faction faces mixed directionality (d ≈ 0.65) — they are targets of extraction through suppression (party discipline, electoral risk) but also gain some negotiating power. The executive branch experiences negative directionality (d ≈ -0.05) — they are partial beneficiaries of the chaos because it expands executive emergency authority and emergency powering of functions. The analytical observer (d ≈ 0.72) sees the constraint as a constitutional design problem with real victims, making it a genuine Tangled Rope rather than a Mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the appropriations mechanism has two genuine structural functions: (1) Coordination — the constitutional requirement for regular appropriations creates a negotiating deadline that forces budget compromise. This is a legitimate coordination function. (2) Extraction — the use of shutdown threat to force policy concessions unrelated to budget disagreement is a coercive mechanism that extracts from victims (federal workers, benefit recipients) to beneficiaries (legislative majority faction). The classification is Snare from the victim perspective because the extraction is pure and the suppression is high. But the classification is Tangled Rope from the analytical perspective because the constraint serves both legitimate coordination (budget negotiation) and illegitimate extraction (policy coercion). The mandatrophy is NOT resolved by claiming the constraint is 'really' Rope (it is not — suppression and extraction are too high). Instead, it is resolved by observing that the constraint's classification changes with the observer's structural position: beneficiaries see Rope, victims see Snare, and the full analytical picture sees Tangled Rope with a design vulnerability that would be prevented by parliamentary confidence voting or automatic budget continuations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shutdown_severity_measurement,
    'What constitutes an actual ''shutdown'' versus administrative mitigation that preserves essential functions?',
    'Temporal analysis of services actually interrupted vs. those continued under carryover authority or emergency powers; correlation between shutdown duration and measurable economic/health impacts',
    'If most services continue: theater ratio increases, classification shifts toward Piton. If cascading failures occur: extraction severity increases, Snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shutdown_severity_measurement, empirical, 'What constitutes functional government shutdown vs. administrative continuity').

omega_variable(
    coercion_mechanism_targeting,
    'Is the shutdown threat calibrated to extract policy concessions, or does it genuinely emerge from budget disagreement?',
    'Temporal analysis of appropriations negotiations; comparison of budget gap size vs. policy concession size; identification of whether threat precedes or follows policy demand',
    'If threat precedes policy demand: pure extraction (Snare confirmed). If threat emerges from genuine budget impasse: coordination failure (Tangled Rope or Rope from multiple perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_targeting, empirical, 'Whether shutdown threat is intentional leverage or emergent negotiation failure').

omega_variable(
    alternative_congressional_mechanism_viability,
    'Could a constitutional amendment or parliamentary reform move appropriations outside the annual brinksmanship cycle?',
    'Comparative constitutionalism; analysis of parliamentary systems (confidence votes, automatic budget continuations); political feasibility assessment of US constitutional reform',
    'If viable alternative exists: constraint is contingent institutional design, not natural. If no viable alternative: constitutional design vulnerability that cannot be easily reformed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_congressional_mechanism_viability, conceptual, 'Whether parliamentary or constitutional alternatives to annual appropriations brinkmanship are feasible').

omega_variable(
    extraction_beneficiary_identification,
    'Who actually benefits from shutdown threats? Legislative majority? Specific policy constituencies? Media and political attention capture?',
    'Policy outcome analysis; tracking which faction succeeds in extracting concessions from appropriations threats over 10+ cycles; identification of patterns in who gains from shutdown-driven negotiations',
    'If clear beneficiary: beneficiary identification refines directionality for legislative actors. If diffuse or contingent: extraction mechanism is less systematic than Snare classification implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_identification, empirical, 'Identification of systematic beneficiaries from shutdown threat mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(appropriations_brinkmanship, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(approp_tr_t0, appropriations_brinkmanship, theater_ratio, 0, 0.35).
narrative_ontology:measurement(approp_tr_t5, appropriations_brinkmanship, theater_ratio, 5, 0.5).
narrative_ontology:measurement(approp_tr_t10, appropriations_brinkmanship, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(approp_be_t0, appropriations_brinkmanship, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(approp_be_t5, appropriations_brinkmanship, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(approp_be_t10, appropriations_brinkmanship, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(appropriations_brinkmanship, enforcement_mechanism).
narrative_ontology:affects_constraint(appropriations_brinkmanship, federal_labor_power_asymmetry).
narrative_ontology:affects_constraint(appropriations_brinkmanship, legislative_minority_veto_mechanism).
narrative_ontology:affects_constraint(appropriations_brinkmanship, emergency_executive_authority_expansion).

% DUAL FORMULATION NOTE:
% The appropriations brinkmanship constraint operates at the intersection of three structural problems: (1) constitutional design vulnerability (annual appropriations deadline), (2) federal labor power asymmetry (workers cannot strike), and (3) executive emergency authority expansion (mitigates actual harm, increases theater ratio). Each downstream constraint has its own extractiveness value and family relationships. The brinkmanship mechanism depends on all three upstream constraints remaining in place.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(appropriations_brinkmanship, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
