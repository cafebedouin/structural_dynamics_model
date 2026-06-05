% ============================================================================
% CONSTRAINT STORY: guinea_junta_legitimization_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guinea_junta_legitimization_2024, []).

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
 *   constraint_id: guinea_junta_legitimization_2024
 *   human_readable: Legitimization of Guinea's 2021 Military Coup
 *   domain: political/state_authority
 *
 * SUMMARY:
 *   Guinea's 2021 military coup, led by Colonel Mamady Doumbouya, formally
 *   overthrew the civilian government of Alpha Condé. The junta established
 *   the National Rally for Development (CNRD) as the governing body and has
 *   progressively formalized its rule through constitutional processes and
 *   institutional theater. This constraint represents the political-coercive
 *   framework by which the junta legitimizes its authority: elections
 *   promised but delayed, constitutional conventions held, international
 *   engagement performed, while simultaneously suppressing opposition,
 *   controlling media, and extracting resources through aligned business
 *   interests. The constraint exhibits signature snare properties: high
 *   suppression (0.78) through security apparatus control; high
 *   extractiveness (0.72) through political imprisonment, asset seizure, and
 *   mining monopoly; rising theater ratio (0.35→0.65) as junta performs
 *   democratic processes to satisfy international legitimacy demands. The
 *   core asymmetry: the junta benefits from indefinite rule stability and
 *   resource monopoly, while civilian political actors and the general
 *   population are trapped with no exit path through law, elections, or
 *   international intervention.
 *
 * KEY AGENTS:
 *   - Military Junta (CNRD): Primary beneficiary (institutional/arbitrage) — controls state apparatus, consolidates coercive authority, gains access to mining wealth and international legitimacy negotiations
 *   - Deposed Civilian Government (Alpha Condé faction): Primary victim (powerless/trapped) — imprisoned leaders, asset seizures, exile, complete exclusion from political process
 *   - Opposition Movements and Political Parties: Victim (moderate/constrained) — banned from organizing, leadership detained, suppressed media access
 *   - General Population and Civil Society: Victim (moderate/constrained) — loss of political voice, security risk from state apparatus, restricted assembly and speech
 *   - Coup-Aligned Mining and Business Interests: Secondary beneficiary (institutional/arbitrage) — protected concessions, state contracts, reduced accountability
 *   - Military Institution (as organizational structure): Hybrid actor (organized/constrained) — enforces extraction while justifying as stability mechanism
 *   - Regional Powers (ECOWAS, France, AU): Conditional beneficiary (institutional/arbitrage) — can arbitrage recognition for compliance with transition timeline
 *   - International Legal System: Institutional observer (institutional/arbitrage) — performs legitimacy recognition despite charter violation, maintains piton function through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guinea_junta_legitimization_2024, 0.72).
domain_priors:suppression_score(guinea_junta_legitimization_2024, 0.78).
domain_priors:theater_ratio(guinea_junta_legitimization_2024, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guinea_junta_legitimization_2024, extractiveness, 0.72).
narrative_ontology:constraint_metric(guinea_junta_legitimization_2024, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(guinea_junta_legitimization_2024, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guinea_junta_legitimization_2024, snare).
narrative_ontology:human_readable(guinea_junta_legitimization_2024, "Legitimization of Guinea's 2021 Military Coup").
narrative_ontology:topic_domain(guinea_junta_legitimization_2024, "political/state_authority").

domain_priors:requires_active_enforcement(guinea_junta_legitimization_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guinea_junta_legitimization_2024, military_junta).
narrative_ontology:constraint_beneficiary(guinea_junta_legitimization_2024, coup_aligned_business_interests).
narrative_ontology:constraint_victim(guinea_junta_legitimization_2024, civilian_political_actors).
narrative_ontology:constraint_victim(guinea_junta_legitimization_2024, opposition_movements).
narrative_ontology:constraint_victim(guinea_junta_legitimization_2024, electoral_democratic_process).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPOSED CIVILIAN ACTORS (SNARE) — Cannot exit the constraint through legal channels. Imprisoned or exiled leaders, banned political parties, suppressed media. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈1.00. Maximum extraction from those who built pre-coup legitimacy.
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GENERAL POPULATION (SNARE) — Constrained by security apparatus and information control. Cannot effectively challenge legitimization narrative. Suppressed freedom of assembly, arrested activists. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.95. Extraction through loss of political voice and security risk.
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL ACTORS (ROPE) — Benefits from transitional legitimacy framework. ECOWAS negotiates junta compliance with timeframes; France gains predictable bilateral relations. Can arbitrage by withholding recognition/aid. d≈0.20, f(d)≈0.08, σ=0.9 → χ≈0.06. Low effective extraction; regional actors can exit.
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MILITARY INSTITUTION (TANGLED ROPE) — Organizes both the extraction apparatus (maintains coercive control) AND a coordination mechanism (stabilizes governance, prevents state collapse). The junta justifies itself as steward against chaos. Constrained by international pressure and coup-fatigue. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.36. Hybrid: enforces legitimization (extraction) while claiming to prevent state failure (coordination).
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COUP-ALIGNED BUSINESS (TANGLED ROPE) — Benefits from junta legitimacy through mining concessions, state contracts, and reduced accountability. Also contributes to coordination by stabilizing business environment. Active enforcement required: junta enforces contract monopolies. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Net beneficiary but dependent on junta survival.
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL SYSTEM (PITON) — Maintains performative legitimacy recognition despite junta hold. Theater_ratio=0.65: junta performs elections/constitutional processes; international system accepts performance as legitimacy. Credentials not challenged at UN. Functionally, international legal authority has atrophied (junta violates charter). d≈0.15, f(d)≈-0.00, σ=1.2 → χ≈-0.00. Institutional inertia maintains recognition despite degraded function.
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (FALSE SUMMIT) — Risks naturalizing junta rule as inevitable state failure management. 'Guinea needed stability; coups are inherent to weak states.' But ε=0.72, suppression=0.78, theater=0.65 contradicts mountain signature (ε≤0.25, suppression≤0.05). Engine flags this as false summit. Junta legitimization is a contingent institutional choice, not a law of nature.
constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guinea_junta_legitimization_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guinea_junta_legitimization_2024, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(guinea_junta_legitimization_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(guinea_junta_legitimization_2024, TR),
    TR >= 0.70.

:- end_tests(guinea_junta_legitimization_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): High and rising. The junta's extraction occurs through multiple mechanisms: political imprisonment (removes competitors), asset seizure (confiscates opposition wealth), mining monopoly (redirects wealth to junta-aligned firms), and international aid capture (state resources flow to security apparatus). The trajectory from 0.45→0.72 reflects consolidation: initially the junta justified coup as temporary correction; by 2024 the junta is extracting through normalized institutions (promised elections, constitutional conventions). The increase is not from rising coercion (that was immediate) but from formalizing extraction into permanent institutional structures. Suppression (0.78): High and structural. Security apparatus (military, national guard, intelligence) suppress opposition through detention, torture allegations, media blackouts, and assembly restrictions. International human rights reports document systematic suppression. Suppression is high because the junta cannot tolerate organized political competition — its legitimacy depends on monopoly control. Theater ratio (0.65): Rising from 0.35. Initial theater was minimal (immediate coup, visible authoritarianism). By 2024, theater has increased substantially: junta performs constitutional processes, promises elections (repeatedly delayed), engages international negotiations, claims transition timeline. Theater serves two functions: satisfies international legitimacy demands and obscures extraction mechanisms. The increase reflects that coercion alone cannot sustain indefinite rule — the junta must perform democratic process to maintain regional acceptance and international recognition. Mandatrophy (resolved): At ε=0.72, the constraint must distinguish between pure extraction (snare) and hybrid coordination-extraction (tangled rope). The junta claims coordination function: 'we stabilize the state, prevent collapse, enable mining investment.' The base_properties declare victims (political actors, civil society) and no genuine beneficiaries outside the junta-business alliance, confirming snare classification. The junta's 'coordination' claim (state stabilization) is instrumentally secondary to extraction — the junta has no interest in including opposition actors in stability arrangements. Pure extraction, not hybrid.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. The deposed civilian actors see a pure snare (trap, no exit, maximal extraction). The general population sees snare with constrained escape (suppressed but not imprisoned; can consider emigration, but at high cost). The military institution sees itself as performing tangled rope (coercive stabilization as coordination function). Coup-aligned business sees rope or even faint coordination benefit (property rights protection). Regional powers see a managed compliance problem (junta performs transition, can be negotiated with). International legal system sees piton (legitimacy recognition persists through institutional inertia despite functional degradation). The analytical observer risks mountain framing ('Guinea needs strong leadership; coups are inevitable in weak states') — but the structural data (extraction rising, suppression high, theater increasing) contradicts this naturalization. The gap reflects that no agent outside the junta-business alliance experiences legitimacy as anything but constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Deposed civilian actors: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Imprisoned leaders cannot exit through law or politics; junta specifically targets former power-holders for asset seizure and exclusion. General population: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction but not maximal — civil society has some evasion options (emigration at cost, underground organizing at risk). Opposition leaders: Victim + constrained → d≈0.90, f(d)≈1.36. Nearly maximal; detention or exile removes most exit options. Military junta: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Can arbitrage by threatening transition reversal or external military action. Coup-aligned business: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Dependent on junta survival but can arbitrage by investing in junta legitimacy or threatening capital flight. Regional powers (ECOWAS/France): Beneficiary + arbitrage → d≈0.25, f(d)≈0.18. Can arbitrage by adjusting aid, recognition, or mediation pressure. International legal system: Institutional + arbitrage → d≈0.12, f(d)≈0.02. Piton classification from theater gate, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy at ε=0.72 concerns whether the junta's claimed 'stability and state protection' function qualifies as coordination that would elevate the classification to tangled rope. Analysis: The junta does provide a narrow coordination function — it prevents immediate state collapse through institutional continuity and resource capture to fund security apparatus. However, this coordination is asymmetrically distributed: it benefits only the junta and its aligned interests. Genuine coordination requires that all parties gain at least some benefit or perceive mutual problem-solving. The junta's 'state protection' is protection against a threat the junta itself created (instability from coup-induced uncertainty). This is not a public good but a private stabilization of the junta's own legitimacy. Furthermore, the junta explicitly excludes opposition actors from any coordination arrangement — there is no institutional mechanism for political competition, coalition-building, or shared governance. Therefore, the constraint is properly classified as snare (pure extraction with coercive monopoly), not tangled rope (mixed coordination-extraction with both beneficiaries and victims sharing some coordination benefit). The mining extraction and international engagement are coordination mechanisms the junta uses instrumentally; they are not coordination with the victims of the constraint. Mandatrophy resolved: Snare confirmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    international_legitimacy_floor,
    'What threshold of international recognition (UN seat, AU membership, bilateral aid) constitutes sufficient legitimacy for junta rule to persist indefinitely?',
    'Longitudinal tracking of sanction escalation vs junta resilience; comparison with historical coup durability in similar international contexts; modeling of aid-withdrawal tipping points',
    'If threshold is low (current trajectory): junta can maintain rule through limited compliance theater (elections, constitutional promises). If threshold requires genuine democratic transition: international pressure becomes extinction mechanism for snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_legitimacy_floor, empirical, 'International legitimacy threshold for junta persistence').

omega_variable(
    elite_cohesion_duration,
    'How long can the junta maintain internal military unity without power-sharing or institutionalized succession?',
    'Analysis of coup leader positioning, factional tensions, statements on transition timeline; comparison with historical patterns of military rule duration in West Africa',
    'If internal cohesion lasts <5 years: constraint collapses into internal coup (snare becomes briefly higher χ then transitions). If >10 years: organizational structure hardens (piton consolidates). If institutionalized (military council, rotation): tangled rope becomes permanent feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_cohesion_duration, empirical, 'Military elite internal cohesion duration').

omega_variable(
    civil_society_organizational_capacity,
    'Can civil society resistance movements reconstitute underground networks despite suppression, creating conditions for mass defection from junta consent?',
    'Tracking of protest frequency and participation despite repression; emergence of coordinated boycotts, strikes, or leaks from state apparatus; international funding of opposition nodes',
    'If civil society capacity grows: snare classification confirmed at broader scales, exit becomes ''constrained'' rather than ''trapped'', enabling collective mobilization. If permanently suppressed: trap persists indefinitely, χ remains maximal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_organizational_capacity, empirical, 'Whether civil society can organize resistance despite suppression').

omega_variable(
    resource_extraction_economic_model,
    'Does junta legitimacy depend critically on mining revenue flows, or can junta sustain rule through coercion alone if mining contracts degrade?',
    'Modeling of state revenue from mining vs military-security spending; analysis of IMF/World Bank loan conditionality impact; tracking of international investment climate for Guinea',
    'If mining-dependent: economic sanctions become deletion mechanism (snare collapses when revenue stops). If coercion-self-sustaining: junta can endure resource depletion (snare becomes orthogonal to economics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_extraction_economic_model, empirical, 'Mining revenue dependency for junta legitimacy maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guinea_junta_legitimization_2024, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gujl_tr_t0, guinea_junta_legitimization_2024, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gujl_tr_t18, guinea_junta_legitimization_2024, theater_ratio, 18, 0.5).
narrative_ontology:measurement(gujl_tr_t36, guinea_junta_legitimization_2024, theater_ratio, 36, 0.65).

% Extraction over time
narrative_ontology:measurement(gujl_be_t0, guinea_junta_legitimization_2024, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gujl_be_t18, guinea_junta_legitimization_2024, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(gujl_be_t36, guinea_junta_legitimization_2024, base_extractiveness, 36, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guinea_junta_legitimization_2024, enforcement_mechanism).
narrative_ontology:affects_constraint(guinea_junta_legitimization_2024, west_african_coup_contagion).
narrative_ontology:affects_constraint(guinea_junta_legitimization_2024, guinea_mining_extraction_monopoly).
narrative_ontology:affects_constraint(guinea_junta_legitimization_2024, ecowas_enforcement_credibility).

% DUAL FORMULATION NOTE:
% Junta legitimization is downstream of the 2021 coup event but represents a distinct structural constraint. The coup itself is an instantaneous break; legitimization is the ongoing institutional framework that perpetuates junta rule. Network dependencies: (1) Coup contagion cluster (Mali, Burkina Faso coups in same period) shares junta legitimization strategies, suggesting common regional constraint pattern. (2) Mining extraction monopoly is instrumentally dependent on junta legitimization — international firms need recognized state authority to formalize concessions. (3) ECOWAS enforcement credibility is affected because regional organization's inability to enforce sanctions or transition timelines degrades its own legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(guinea_junta_legitimization_2024, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
