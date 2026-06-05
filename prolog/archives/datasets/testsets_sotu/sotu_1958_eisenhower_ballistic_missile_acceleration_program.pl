% ============================================================================
% CONSTRAINT STORY: sotu_1958_eisenhower_ballistic_missile_acceleration_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1958_eisenhower_ballistic_missile_acceleration_program, []).

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
 *   constraint_id: sotu_1958_eisenhower_ballistic_missile_acceleration_program
 *   human_readable: Accelerated Ballistic Missile Procurement and Development (1957-1960)
 *   domain: military/defense_procurement
 *
 * SUMMARY:
 *   The 1958 SOTU address on accelerated ballistic missile development
 *   represents a structural constraint that operates simultaneously as (1) a
 *   genuine coordination mechanism for maintaining strategic deterrent
 *   credibility, (2) an extraction mechanism benefiting defense contractors
 *   through cost-plus procurement, and (3) a ritualized institutional
 *   response to perceived Soviet capabilities that may have been overstated.
 *   The acceleration shifted U.S. strategic doctrine from air-breathing
 *   (B-47, B-52 bombers) to land-based (Atlas, Titan) and submarine-based
 *   (Polaris) systems, requiring coordination of technical milestones across
 *   multiple contractors and military branches. The constraint exhibits a
 *   perspectival spectrum: Soviet leadership experiences it as a compulsory
 *   escalatory trap with no exit (Snare), competing U.S. military branches
 *   experience mixed coordination benefits and budget extraction (Tangled
 *   Rope), defense contractors experience pure coordination with favorable
 *   arbitrage terms (Rope), NATO allies experience a temporary security
 *   guarantee with a sunset clause (Scaffold), the Strategic Air Command
 *   experiences genuine deterrent coordination alongside operational burden
 *   (Tangled Rope), and the civilizational observer risks naturalizing
 *   contingent Cold War politics as immutable strategic law (false-summit
 *   Mountain). The constraint's theater ratio (0.48 at t=0, rising to 0.52 by
 *   t=6) indicates that while technical development is genuine, significant
 *   institutional performance is directed at demonstrating deterrent
 *   credibility to domestic and Soviet audiences rather than achieving
 *   marginal additional capability.
 *
 * KEY AGENTS:
 *   - Defense Contractors (Convair, Martin Marietta, Lockheed, Aerojet, Bell Labs): Institutional beneficiaries (arbitrage exit) — primary extraction recipients through cost-plus procurement, technical direction authority, and guaranteed contracts
 *   - Strategic Air Command: Institutional actor (constrained exit) — both benefits from coordinated deterrent architecture and bears operational burden of dual-platform transition
 *   - Soviet Command Structure: Powerless victim (trapped exit) — faces compulsory matching response with no coordination benefit; escalation is existentially required
 *   - Competing U.S. Military Branches (Army, Navy): Moderate victims (constrained exit) — bear asymmetric budget extraction as Air Force strategic programs consume disproportionate resources
 *   - Domestic Budget Priorities: Victim (trapped exit) — abstract collective good displaced by accelerated procurement; education, infrastructure, healthcare spending deprioritized
 *   - NATO Allies: Organized beneficiaries (constrained exit) — receive temporary extended deterrent security; can coordinate on alternatives (UK missile, French nuclear program)
 *   - Congress/Executive Branch: Institutional organizer (institutional exit) — enforces acceleration through authorization and appropriations; captures political legitimacy through deterrent rhetoric
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1958_eisenhower_ballistic_missile_acceleration_program, 0.58).
domain_priors:suppression_score(sotu_1958_eisenhower_ballistic_missile_acceleration_program, 0.72).
domain_priors:theater_ratio(sotu_1958_eisenhower_ballistic_missile_acceleration_program, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1958_eisenhower_ballistic_missile_acceleration_program, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1958_eisenhower_ballistic_missile_acceleration_program, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1958_eisenhower_ballistic_missile_acceleration_program, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1958_eisenhower_ballistic_missile_acceleration_program, tangled_rope).
narrative_ontology:human_readable(sotu_1958_eisenhower_ballistic_missile_acceleration_program, "Accelerated Ballistic Missile Procurement and Development (1957-1960)").
narrative_ontology:topic_domain(sotu_1958_eisenhower_ballistic_missile_acceleration_program, "military/defense_procurement").

domain_priors:requires_active_enforcement(sotu_1958_eisenhower_ballistic_missile_acceleration_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1958_eisenhower_ballistic_missile_acceleration_program, defense_contractors).
narrative_ontology:constraint_beneficiary(sotu_1958_eisenhower_ballistic_missile_acceleration_program, military_air_command).
narrative_ontology:constraint_beneficiary(sotu_1958_eisenhower_ballistic_missile_acceleration_program, strategic_deterrent_capability).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_ballistic_missile_acceleration_program, domestic_budget_allocation).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_ballistic_missile_acceleration_program, competing_military_branches).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_ballistic_missile_acceleration_program, soviet_economic_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET STRATEGIC RESPONSE (SNARE) — Trapped in an escalatory spiral with no exit. Soviet leadership perceives the accelerated U.S. ICBM program as an existential threat to strategic parity. Response capacity is constrained by industrial capacity and technological lag. Escalation is compulsory — failure to match U.S. deployment risks strategic vulnerability. The constraint extracts constant mobilization of Soviet resources (Semyorka, R-16 programs) with no coordination benefit, only matching moves. Maximum experienced extraction from a powerless position — no choice but to compete.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING MILITARY BRANCHES (TANGLED ROPE) — Army, Navy, Air Force, and Strategic Air Command compete for resource allocation within the acceleration program. Each branch benefits from expanded defense budgets (coordination function) but suffers asymmetric extraction as Air Force strategic programs consume disproportionate resources. Constrained by political necessity to participate in deterrent coordination while losing budget share to competitor branches. Mixed coordination (deterrent mission) and extraction (internal budget competition).
constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE CONTRACTORS (ROPE) — Primary beneficiary. Acceleration program provides guaranteed contracts, cost-plus procurement, and coordination of technical standards across multiple missile programs (Atlas, Titan, Thor, Jupiter). Effective arbitrage: contractors can reallocate resources across programs without exit cost. Experiences constraint as pure coordination mechanism — the acceleration enforces synchronization of technical milestones and production schedules, enabling contractors to optimize manufacturing capacity. Net negative effective extraction — constraint subsidizes contractor operations.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STRATEGIC AIR COMMAND (TANGLED ROPE) — Genuine coordination benefit: acceleration program resolves coordination problem of maintaining continuous airborne alert (SAC bombers) while transitioning to land-based ICBMs. Asymmetric extraction: SAC must absorb operational burden of maintaining dual-platform deterrent (manned bombers + missiles) during transition period. Constrained exit: cannot choose slower transition without compromising deterrent posture. Both coordination (integrated deterrent architecture) and extraction (operational burden) present.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NATO ALLIES (SCAFFOLD) — Acceleration program provides temporary security guarantee through U.S. ICBM superiority (1957-1965). Organized capacity to coordinate on deterrent architecture. Sees constraint as temporary: once ICBMs are deployed and Soviet response capability stabilizes, the accelerated tempo is no longer required. Sunset clause implicit: deterrent credibility is the exit condition. Suppression is moderate because NATO allies can coordinate on alternative deterrent architectures (e.g., UK missile development, French nuclear program).
constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STRATEGIC DETERRENCE DOCTRINE (PITON) — The acceleration program institutionalizes 'mutually assured destruction' as a civilizational constraint. By 1965, MAD doctrine persists through institutional inertia despite ongoing technological change (MIRVs, ABM systems, targeting precision improvements). The doctrine's primary function (preventing nuclear war) has decayed into a performative ritual of nuclear balance-sheet accounting and parity calculations. Theater ratio reflects that much of the deterrent force is maintained through habit and institutional momentum rather than active deterrent calculus. Piton classification: the constraint persists because the institutional apparatus treats it as immutable, not because deterrent logic requires continuous acceleration.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the acceleration program appears as an inevitable consequence of arms race dynamics: once one power achieves technological superiority, competitors must match it or risk existential vulnerability. This perspective treats the ICBM acceleration as an immutable law of strategic competition — a constraint no actor can escape regardless of preferences. However, the structural data reveals this as a false summit: the constraint has identifiable beneficiaries (defense contractors, military-industrial complex) and bears the signature of constructed institutional arrangements rather than natural law. The naturalization of arms race escalation serves the interests of those who benefit.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1958_eisenhower_ballistic_missile_acceleration_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1958_eisenhower_ballistic_missile_acceleration_program, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1958_eisenhower_ballistic_missile_acceleration_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1958_eisenhower_ballistic_missile_acceleration_program, TR),
    TR >= 0.70.

:- end_tests(sotu_1958_eisenhower_ballistic_missile_acceleration_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The acceleration program extracts resources from competing military branches, domestic budget priorities, and Soviet competitors through sustained escalation. However, extraction is not maximal because genuine coordination function exists: deterrent credibility requires synchronized development across multiple contractors and services. The trajectory shows rising extractiveness (0.32→0.58) reflecting accumulating institutional commitments and contractor dependence. Suppression (0.72): High. Significant barriers to exit include geopolitical constraints (deterrent credibility requires visible deployment), contractor political power (congressional districts, union employment), military institutional lock-in, and perceptual constraints (belief that failure to accelerate risks strategic vulnerability). Soviet competitors face absolute suppression — no exit from matching response. Theater ratio (0.48-0.52): Moderate and rising. Technical development is genuine (ballistic missiles require real engineering), but significant portion of activity is directed at demonstrating deterrent credibility, generating Congressional support, and signaling resolve to adversaries. The rising theater ratio indicates increasing institutional performance as genuine technical barriers are overcome (Atlas deployment 1959) and focus shifts to political sustainment of funding.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximum perspectival divergence across indices. The defense contractor (institutional/arbitrage/immediate/national) sees Rope — pure coordination of technical requirements with favorable financial terms. The Soviet competitor (powerless/trapped/biographical/global) sees Snare — compulsory escalation with no coordination benefit. The Strategic Air Command (institutional/constrained/biographical/national) sees Tangled Rope — deterrent coordination mission alongside operational burden. The competing branch (moderate/constrained/biographical/national) sees Tangled Rope — budget competition within a shared deterrent requirement. NATO allies (organized/constrained/generational/global) see Scaffold — temporary security guarantee with visible sunset as Soviet deployment reaches parity. The journal editorial system analog (piton) would be the institutional inertia of deterrent doctrine — the continued acceleration even after technical barriers are overcome and Soviet parity is achieved. The civilizational observer (analytical/analytical/civilizational/global) risks seeing Mountain — arms race escalation as inevitable law — but structural data reveals false summit: beneficiary list and budget crowding-out mechanisms indicate constructed constraint, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary sharply by actor structural position. Defense contractors (institutional/arbitrage) derive d ≈ 0.10-0.20 (beneficiary with low-cost exit) → negative or near-zero f(d) → low or negative experienced extraction. Soviet competitors (powerless/trapped) derive d ≈ 0.95 (victim with zero exit) → high f(d) → maximum experienced extraction. Competing U.S. branches (moderate/constrained) derive d ≈ 0.70 (victim with high-cost exit) → elevated f(d) → moderate extraction. Strategic Air Command (institutional/constrained) derives d ≈ 0.65 (mixed beneficiary/victim status, significant exit costs) → elevated f(d) → moderate-high extraction. NATO allies (organized/constrained) derive d ≈ 0.55 (mixed beneficiary status, some exit options via independent programs) → moderate f(d) → moderate extraction. The perspectival gaps reflect real structural differences in how the constraint's benefits and costs flow: beneficiaries cluster at d ≤ 0.25; victims cluster at d ≥ 0.65. Scope modifier σ(S) = 1.2 (global): escalation dynamics are worldwide, amplifying effective extraction for all non-beneficiary positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint performs genuine coordination (deterrent credibility) while simultaneously extracting from multiple victim groups (Soviet competitors, competing military branches, domestic budget priorities). The tangled rope classification preserves this hybrid: base extraction ε=0.58 (high), beneficiaries present (defense contractors, strategic deterrent capability), victims present (multiple), active enforcement required (Congressional authorization, military command structure), suppression high (geopolitical constraints, contractor political power). The constraint cannot be classified as pure Rope because extraction is asymmetric (beneficiaries gain more than victims gain from coordination benefits). It cannot be classified as pure Snare because genuine coordination function exists (deterrent architecture requires technical synchronization and integration). The false-summit Mountain perspective attempts to naturalize the constraint ('arms races are inevitable') but is contradicted by beneficiary presence (defense contractors benefit in specific, identifiable ways) and measured theater ratio (institutional performance is directed at political legitimacy, not technical necessity). Mandatrophy resolves: this is a Tangled Rope constrained by geopolitical necessity, not a Mountain constrained by law of physics or strategy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_sufficiency_threshold,
    'What level of ICBM deployment constitutes sufficient strategic deterrent, and at what point does further acceleration become rent-seeking rather than coordination?',
    'Post-hoc analysis: compare actual Soviet ICBM deployment rates (1960-1965) with U.S. threat assessments (1957-1959). Calculate whether actual threat magnitude justified acceleration rate.',
    'If threshold is lower than 1957-1959 estimates: acceleration was partially unnecessary extraction. If threshold matches estimates: acceleration was justified coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrent_sufficiency_threshold, empirical, 'Threshold for deterrent sufficiency vs. unnecessary acceleration').

omega_variable(
    soviet_response_capability_lag,
    'Was the Soviet ICBM program genuinely lagging U.S. capability in 1957, or were intelligence assessments inflated to justify acceleration spending?',
    'Declassified intelligence assessments (CIA) vs. actual Soviet deployment timelines. Analysis of missile gap hypothesis accuracy.',
    'If gap was real and imminent: acceleration justified. If gap was exaggerated: acceleration was driven by procurement politics rather than genuine threat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soviet_response_capability_lag, empirical, 'Whether Soviet ICBM lag justified acceleration intensity').

omega_variable(
    budget_crowding_out_mechanism,
    'What domestic spending was displaced by the $1 billion+ annual ICBM acceleration? How permanent were those budget reallocations?',
    'Federal budget analysis 1957-1965: tracking of education, infrastructure, healthcare spending trends. Comparison with counterfactual baseline projections.',
    'If displacement is permanent: extraction cost persists beyond deterrent window. If displacement reverses: extraction is temporary (consistent with Scaffold classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(budget_crowding_out_mechanism, empirical, 'Permanence of budget crowding-out effects').

omega_variable(
    contractor_profit_extraction,
    'What proportion of the acceleration program''s cost was genuine technical development vs. cost-plus profit extraction by defense contractors?',
    'Defense contract audits; cost analysis of Atlas, Titan, Thor, Jupiter programs. Comparison of cost overruns across programs.',
    'If profit extraction is high (>25% of program cost): defense contractors are primary beneficiaries and extraction is severe. If profit extraction is low (<15%): beneficiary gains are legitimate coordination rewards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractor_profit_extraction, empirical, 'Contractor profit extraction within acceleration program').

omega_variable(
    deterrent_credibility_window,
    'For how long did the accelerated ICBM deployment actually maintain U.S. strategic superiority, and when did the constraint transition from deterrent necessity to institutional inertia?',
    'Strategic force composition analysis: deployment dates of Atlas (1959), Titan (1962), Polaris (1960). Soviet response timeline (R-7, R-16). Window closes when Soviet missiles reach parity.',
    'If window closes by 1965: Scaffold sunset is real, piton classification is appropriate post-1965. If window extends beyond 1970: extraction persists longer, tangled_rope characterization applies to broader period.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrent_credibility_window, empirical, 'Duration of U.S. ICBM strategic superiority window').

omega_variable(
    false_summit_marker,
    'Does the ''immutable law of arms race escalation'' (Mountain perspective) naturalize what is actually a contingent institutional arrangement driven by contractor interests and Cold War politics?',
    'Counterfactual analysis: would acceleration have occurred at same pace without defense contractor lobbying? Comparison with UK/French nuclear programs that proceeded at slower rates. Examination of intelligence assessments predating public justifications.',
    'If naturalization is confirmed: Mountain classification is a false summit. Constraint is Tangled Rope misrepresented as Mountain to obscure beneficiary extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_marker, conceptual, 'Whether arms race escalation is natural law or constructed constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1958_eisenhower_ballistic_missile_acceleration_program, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icbm_tr_t0, sotu_1958_eisenhower_ballistic_missile_acceleration_program, theater_ratio, 0, 0.32).
narrative_ontology:measurement(icbm_tr_t2, sotu_1958_eisenhower_ballistic_missile_acceleration_program, theater_ratio, 2, 0.4).
narrative_ontology:measurement(icbm_tr_t4, sotu_1958_eisenhower_ballistic_missile_acceleration_program, theater_ratio, 4, 0.48).
narrative_ontology:measurement(icbm_tr_t6, sotu_1958_eisenhower_ballistic_missile_acceleration_program, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(icbm_be_t0, sotu_1958_eisenhower_ballistic_missile_acceleration_program, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(icbm_be_t2, sotu_1958_eisenhower_ballistic_missile_acceleration_program, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(icbm_be_t4, sotu_1958_eisenhower_ballistic_missile_acceleration_program, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(icbm_be_t6, sotu_1958_eisenhower_ballistic_missile_acceleration_program, base_extractiveness, 6, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1958_eisenhower_ballistic_missile_acceleration_program, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_ballistic_missile_acceleration_program, soviet_icbm_development_response).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_ballistic_missile_acceleration_program, defense_budget_allocation_compression).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_ballistic_missile_acceleration_program, military_industrial_complex_institutional_lock_in).

% DUAL FORMULATION NOTE:
% The acceleration program is decomposed into three structurally distinct constraints: (1) the U.S. accelerated development program (this story, ε=0.58, Tangled Rope), (2) the Soviet compulsory matching response (downstream, ε≈0.75, Snare), (3) the institutional inertia of deterrent doctrine post-deployment (piton, ε≤0.25, theater≥0.70). Each has distinct ε values reflecting different structural mechanisms. The U.S. program coordinates deterrent deployment while extracting from budget competitors. The Soviet response is pure extraction (no coordination benefit for response-forced actors). The piton represents the persistence of deterrent logic after technical barriers are overcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1958_eisenhower_ballistic_missile_acceleration_program, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
