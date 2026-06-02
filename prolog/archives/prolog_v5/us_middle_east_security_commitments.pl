% ============================================================================
% CONSTRAINT STORY: us_middle_east_security_commitments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_middle_east_security_commitments, []).

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
 *   constraint_id: us_middle_east_security_commitments
 *   human_readable: US Middle East Security Commitments
 *   domain: geopolitical/security
 *
 * SUMMARY:
 *   US Middle East security commitments represent a hybrid
 *   coordination-extraction mechanism spanning military alliances, defense
 *   treaties, forward-deployed forces, and security guarantees to regional
 *   state actors. The constraint emerged from Cold War containment doctrine
 *   and post-9/11 counterterrorism strategy, creating a structural framework
 *   where the US maintains military presence, intelligence operations, arms
 *   sales, and security guarantees to allied governments (Saudi Arabia, UAE,
 *   Israel, Egypt, Jordan) while bearing fiscal costs and military personnel
 *   exposure. The arrangement exhibits genuine coordination functions
 *   (intelligence sharing, deterrence of regional great-power competition,
 *   humanitarian military capabilities) alongside asymmetric extraction
 *   (regional allies trade sovereignty for security guarantees; US fiscal
 *   resources diverted from domestic use; regional civilian populations
 *   absorb military operation collateral damage). The theater ratio (0.68)
 *   reflects significant performative elements: base ceremonies, military
 *   exercises, and threat inflation narratives sustain institutional presence
 *   even as strategic rationale has degraded post-Cold War. The constraint is
 *   actively enforced through military bureaucracy, alliance treaties, and
 *   security doctrine but faces pressure from competing narratives (foreign
 *   policy restraint, fiscal priorities, regional power redistribution,
 *   diplomatic alternatives). The extractiveness trajectory (0.35 → 0.58 over
 *   45 years) shows accumulation of costs and theater as the original
 *   strategic rationale (Soviet containment) ended and new justifications
 *   (terrorism, regional stability, great-power competition) required
 *   continuous commitment renewal.
 *
 * KEY AGENTS:
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — maintains force projection capability, bases, weapons sales, and institutional funding through commitment structure
 *   - Regional Allied Governments: Secondary beneficiary/victim (moderate/constrained) — gain security guarantees and weapons supply; constrained by security dependence and alignment requirements
 *   - US Fiscal Authority (Department of Defense, Congress): Primary victim (powerful/mobile) — bears extraction through defense spending ($50-80B annually in direct Middle East operations); theoretically mobile but constrained by path dependency and political economy
 *   - Regional Civilian Populations: Primary victim (powerless/trapped) — bear extraction through military operations, drone strikes, and proxy warfare with no exit options or voice in strategic frameworks
 *   - Non-aligned Regional Actors: Secondary victim (moderate/constrained) — excluded from security arrangements, face containment/deterrence pressure, constrained by US-allied military superiority
 *   - Cold War Institutional Legacy: Beneficiary (institutional/arbitrage) — strategic doctrine that benefits from commitment continuation through bureaucratic inertia; sees own degradation as theater but maintains institutional framework
 *   - International Peace/Diplomatic Community: Organized opposition (organized/constrained) — perceive commitment as suppressant of diplomatic alternatives; constrained by state-centric security system and great-power politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_middle_east_security_commitments, 0.58).
domain_priors:suppression_score(us_middle_east_security_commitments, 0.62).
domain_priors:theater_ratio(us_middle_east_security_commitments, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_middle_east_security_commitments, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_middle_east_security_commitments, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_middle_east_security_commitments, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_middle_east_security_commitments, tangled_rope).
narrative_ontology:human_readable(us_middle_east_security_commitments, "US Middle East Security Commitments").
narrative_ontology:topic_domain(us_middle_east_security_commitments, "geopolitical/security").

domain_priors:requires_active_enforcement(us_middle_east_security_commitments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_middle_east_security_commitments, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_middle_east_security_commitments, regional_allied_governments).
narrative_ontology:constraint_beneficiary(us_middle_east_security_commitments, us_strategic_power_projection).
narrative_ontology:constraint_victim(us_middle_east_security_commitments, us_fiscal_stability).
narrative_ontology:constraint_victim(us_middle_east_security_commitments, regional_civilian_populations).
narrative_ontology:constraint_victim(us_middle_east_security_commitments, non_aligned_regional_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL CIVILIAN POPULATIONS (SNARE) — Trapped in conflict zones where US security commitments escalate military operations. Bear the direct costs of interventions, drone strikes, and proxy wars without meaningful exit options or voice in the strategic frameworks. Maximum experienced extraction with no coordination benefit.
constraint_indexing:constraint_classification(us_middle_east_security_commitments, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL ALLIED GOVERNMENT (TANGLED ROPE) — Benefits from US security guarantees against regional rivals; coordinates defense strategy and intelligence sharing. Constrained by dependence on US military aid and political alignment requirements. Mixed coordination (genuine security cooperation) and asymmetric extraction (sovereignty constraints, pressure on domestic policy).
constraint_indexing:constraint_classification(us_middle_east_security_commitments, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US MILITARY INSTITUTIONAL FRAMEWORK (ROPE) — Experiences commitment structure as enabling coordination: forward bases, treaty alliances, and security guarantees coordinate deterrence strategies and information sharing across regional actors. Benefits from institutional continuity, force projection capability, and alliance network maintenance. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(us_middle_east_security_commitments, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US FISCAL AUTHORITY AND TAXPAYERS (TANGLED ROPE) — Bear extraction through defense spending (estimated $50-80 billion annually in direct Middle East military operations plus opportunity costs). Powerful agents can theoretically exit through democratic process or policy change, but exit is constrained by path dependency, bureaucratic momentum, and normalized security discourse. Mixed: coordination of regional stability benefits domestic security; extraction occurs through fiscal burden and opportunity costs.
constraint_indexing:constraint_classification(us_middle_east_security_commitments, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL PEACE COALITION (SCAFFOLD) — Organized actors (UN bodies, international NGOs, anti-war movements) perceive the commitment structure as a temporary institutional arrangement with potential sunset mechanisms: nuclear negotiations (JCPOA model), regional diplomacy initiatives, and arms control frameworks represent exit pathways. Constraint perceived as solvable through institutional reform with declining extraction over time.
constraint_indexing:constraint_classification(us_middle_east_security_commitments, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR SECURITY ORTHODOXY (PITON) — The commitment framework persists as institutional theater: forward basing, alliance maintenance, and containment doctrine reflect post-WWII/Cold War institutional patterns that have atrophied in functional relevance but remain maintained through bureaucratic inertia. Theater ratio high (military exercises, base ceremonies, threat inflation) with degraded functional purpose. The constraint exists because the institutions that created it have not been replaced, not because the original strategic rationale remains compelling.
constraint_indexing:constraint_classification(us_middle_east_security_commitments, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - STRUCTURAL ENTRAPMENT VIEW (SNARE) — From civilizational analysis, the commitment framework exhibits classic snare characteristics: high suppression of alternatives (diplomatic options de-emphasized in strategic discourse), high extraction (military spending, opportunity costs, regional instability perpetuation), minimal coordination benefit (containment rationale degraded post-Cold War), and sustained through narrative enforcement (terrorism threat, humanitarian intervention framing). The constraint persists not because no exit exists but because the exit mechanisms are suppressed.
constraint_indexing:constraint_classification(us_middle_east_security_commitments, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_middle_east_security_commitments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_middle_east_security_commitments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_middle_east_security_commitments, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_middle_east_security_commitments, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_middle_east_security_commitments, TR),
    TR >= 0.70.

:- end_tests(us_middle_east_security_commitments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts substantial fiscal resources (~$50-80B annually estimated direct Middle East military costs plus opportunity costs), creates military personnel exposure and casualties, and perpetuates regional instability that requires continued intervention. However, extractiveness is not maximum (≥0.70) because genuine coordination functions exist (deterrence of regional great-power competition, intelligence collaboration, humanitarian military capabilities). The measurement trajectory shows steady increase from 0.35 (early Cold War, clear strategic rationale) to 0.58 (contemporary, degraded rationale but sustained through institutional inertia). Suppression (0.62): Moderate-high. The constraint suppresses alternatives through: (1) security discourse normalization (terrorism threat inflation, regional stability framing), (2) institutional path dependency (military budget allocation patterns, base infrastructure), (3) alliance lock-in (treaties, weapons dependency), (4) information asymmetry (classified threat assessments), and (5) political economy (defense industry lobbying, military employment in key congressional districts). Suppression is not total because democratic exit mechanisms technically exist and are periodically activated (withdrawal debates, arms sales challenges). Theater ratio (0.68): High and increasing. Significant performative elements include: (1) base ceremonies and flag exercises, (2) threat narratives exceeding actual threat capacity, (3) continued commitment justifications shifting as original rationale (Soviet containment) became obsolete, (4) presence operations maintaining institutional visibility rather than deterrence function. The trajectory from 0.52 to 0.68 indicates increasing theater proportion as functional necessity has declined and institutional maintenance has become primary driver.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's true nature as asymmetric. When the military institutional framework evaluates the commitment as Rope (pure coordination), they are measuring from within the benefit structure. When regional civilian populations evaluate it as Snare, they are measuring from outside that benefit structure, bearing costs without compensation. The gap is not uncertainty — it is proof of extraction. If the constraint were pure Rope, all perspectives would converge on Rope (as with true coordination problems like language standards). The systematic divergence proves that the constraint has an asymmetric structure that concentrates benefits toward institutional actors and costs toward powerless populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. US military institutions (institutional power, arbitrage exit) have directionality d ≈ 0.15-0.20, producing negative f(d) and negative effective extraction chi — the constraint flows toward them as benefit. Regional allied governments (moderate power, constrained exit) have d ≈ 0.55-0.65, producing moderate f(d) ≈ 0.70-0.90, moderate-high chi — they experience meaningful extraction despite some benefits. Regional civilians (powerless, trapped exit) have d ≈ 0.95, producing maximum f(d) ≈ 1.42, maximum experienced extraction chi — they bear full cost with no exit. US fiscal authorities (powerful agents, but constrained by political economy within security framework) have d ≈ 0.60-0.75 depending on whether their exit capacity is evaluated as mobile (theoretically) or constrained (practically), producing moderate-high f(d) and chi. The beneficiary/victim declarations (military-industrial-complex as beneficiary; fiscal stability and civilian populations as victims) ensure that the directionality pipeline flows extraction toward the institutional actors and away from powerless/trapped populations, which matches the structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three Tangled Rope gates: (1) Beneficiaries declared (US military institutional framework, regional allied governments, military-industrial complex) — genuine coordination function exists (deterrence, intelligence sharing, regional stability coordination). (2) Victims declared (fiscal stability, regional civilian populations, non-aligned actors) — asymmetric extraction proven by cost concentration and powerless populations. (3) Active enforcement required and present — military institutions, security treaties, alliance frameworks actively maintain the constraint. The mandatrophy resolves by acknowledging that the constraint is genuinely both: it coordinates regional deterrence systems (legitimate coordination function) while simultaneously extracting resources, constraining sovereignty, and perpetuating conflict that could be resolved through alternative diplomatic mechanisms. The classification does not excuse the extraction; it specifies its structure: the extraction is not incidental to coordination (which would make it pure Snare) but is constitutive of the coordination mechanism itself. Regional actors receive security guarantees that genuinely deter threats AND simultaneously lose sovereignty and must accept US strategic priorities. This hybrid structure is precisely what Tangled Rope captures: coordination that works for beneficiaries precisely because it extracts from victims. The mandatrophy prevents mislabeling this as either pure coordination (Rope, ignoring victim costs) or pure extraction (Snare, ignoring genuine deterrence functions).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_necessity_vs_path_dependency,
    'Are US Middle East security commitments driven by genuine strategic necessity or by institutional path dependency and sunk-cost mentality?',
    'Comparative analysis of declared threats vs. actual threat capacity; scenario modeling of withdrawal consequences vs. status quo costs; assessment of alternative deterrence mechanisms (naval presence, cyber capacity, drone-based deterrence)',
    'If necessity: constraints may classify higher in coordination functions (rope/tangled rope dominates). If path dependency: constraints classify higher in extraction (snare/piton dominates). Resolution changes mandatrophy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_necessity_vs_path_dependency, conceptual, 'Strategic necessity vs institutional path dependency in commitment persistence').

omega_variable(
    regional_alliance_stability_without_us_commitment,
    'Would regional state actors maintain militarized postures and mutual deterrence absent explicit US security guarantees, or would de-escalation occur?',
    'Historical analysis of pre-US-involvement regional dynamics; current modeling of incentive structures if US withdraws; comparison with regions with minimal US military presence (Southeast Asia pre-pivot, Latin America)',
    'If escalation inevitable: US commitment provides genuine coordination benefit (tangled rope, rope dominant). If de-escalation likely: commitment structure suppresses peaceful alternatives (snare dominant, coordination function minimal).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_alliance_stability_without_us_commitment, conceptual, 'Whether regional stability depends on US security commitments or suppresses local peace mechanisms').

omega_variable(
    theater_vs_functional_security_operations,
    'What proportion of US military posturing in the Middle East (base maintenance, exercise operations, presence patrols) serves functional security objectives versus institutional theater and presence projection?',
    'Operational audit of force posture; cost-benefit analysis of specific bases and operations; comparison of theater costs vs strategic output; assessment of what threat reduction actually results from current deployment levels',
    'If functional >70%: piton classification invalid, constraint leans tangled_rope/snare. If theater >60%: piton classification confirmed, institutional inertia is primary driver.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_vs_functional_security_operations, empirical, 'Proportion of operations that are functional versus theatrical').

omega_variable(
    extraction_direction_ambiguity,
    'Does the commitment framework extract from the US (fiscal burden, military casualties, opportunity costs) or from regional actors (sovereignty constraints, military dependency, civilian casualties from operations)?',
    'Comparative cost accounting: direct military spending + opportunity costs vs regional states'' military spending increases + arms dependency vs civilian casualties in US-supported operations',
    'If extraction primarily toward US: victim classification shifts to powerful US taxpayers (tangled rope). If extraction primarily from region: victim classification remains civilian populations (snare). Beneficiary classification also affected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_direction_ambiguity, empirical, 'Direction and magnitude of extraction across beneficiary/victim groups').

omega_variable(
    identity_lock_in_security_doctrine,
    'Is US institutional commitment to Middle East security presence driven by strategic calculation or by identity fusion (US identity as global hegemon, military-institutional identity constituted through forward deployment)?',
    'Discourse analysis of policy justifications (rational threat-response vs identity-affirmation framing); comparison of stated threat assessments vs actual threat data; analysis of withdrawal resistance (structural cost vs cognitive/identity cost)',
    'If identity-locked: institutional actors should be classified with identity_locked exit option rather than arbitrage, changing directionality and chi calculations substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_security_doctrine, conceptual, 'Whether commitment reflects strategic calculation or identity fusion in US military doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_middle_east_security_commitments, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usmes_tr_t0, us_middle_east_security_commitments, theater_ratio, 0, 0.52).
narrative_ontology:measurement(usmes_tr_t15, us_middle_east_security_commitments, theater_ratio, 15, 0.61).
narrative_ontology:measurement(usmes_tr_t30, us_middle_east_security_commitments, theater_ratio, 30, 0.68).
narrative_ontology:measurement(usmes_tr_t45, us_middle_east_security_commitments, theater_ratio, 45, 0.74).

% Extraction over time
narrative_ontology:measurement(usmes_be_t0, us_middle_east_security_commitments, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usmes_be_t15, us_middle_east_security_commitments, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(usmes_be_t30, us_middle_east_security_commitments, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(usmes_be_t45, us_middle_east_security_commitments, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_middle_east_security_commitments, enforcement_mechanism).
narrative_ontology:affects_constraint(us_middle_east_security_commitments, israeli_palestinian_security_paradigm).
narrative_ontology:affects_constraint(us_middle_east_security_commitments, saudi_iran_regional_competition).
narrative_ontology:affects_constraint(us_middle_east_security_commitments, gulf_state_arms_dependency).
narrative_ontology:affects_constraint(us_middle_east_security_commitments, us_energy_security_commitment).

% DUAL FORMULATION NOTE:
% US Middle East security commitments are upstream of multiple regional conflict dynamics and energy security constraints. The commitment framework structurally enables (or necessitates) allied states' military posturing and constrains non-aligned regional actors' strategic options. Changes to the commitment structure would cascade through all downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_middle_east_security_commitments, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
