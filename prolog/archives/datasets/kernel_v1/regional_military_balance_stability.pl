% ============================================================================
% CONSTRAINT STORY: regional_military_balance_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_military_balance_stability, []).

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
 *   constraint_id: regional_military_balance_stability
 *   human_readable: Regional Military Balance Stability Constraint
 *   domain: geopolitical/security/military
 *
 * SUMMARY:
 *   Regional military balance stability represents a foundational constraint
 *   in multipolar geopolitical systems. The constraint emerges from the
 *   security dilemma: when no higher authority can enforce agreements, each
 *   state's defensive military preparations create incentive structures that
 *   produce arms races regardless of actors' explicit intentions. This
 *   structural logic generates genuine coordination problems (preventing war,
 *   maintaining deterrence credibility) alongside profound asymmetric
 *   extraction (diversion of resources to military spending, subordination of
 *   subordinate states to hegemonic preferences, institutional capture by
 *   military-industrial interests). The constraint exhibits all six DR types
 *   from different structural positions: the security dilemma appears as
 *   immutable natural law to the civilizational observer (mountain), but
 *   reveals itself as contingent institutional arrangement when examined
 *   through subordinate states (snare), rival powers (tangled rope), hegemons
 *   (rope), peace coalitions (scaffold), and degraded doctrinal frameworks
 *   (piton). The metrics show extraction accumulation over 20 years (0.38 →
 *   0.58), reflecting how military competition layers additional extraction
 *   mechanisms (new weapons systems, expanded military bases, parallel
 *   sanctions evasion networks) onto the original coordination problem.
 *   Theater ratio rises (0.52 → 0.68), indicating increasing performative
 *   content: strategic doctrines invoke 'balance' and 'stability' despite
 *   changing threat environments (cyber, asymmetric, non-state) that the
 *   traditional framework cannot address. Suppression requirement intensifies
 *   (0.58 → 0.72) as enforcement of military spending discipline, arms
 *   embargo compliance, and non-proliferation regimes requires greater
 *   institutional capacity.
 *
 * KEY AGENTS:
 *   - Subordinate States: Primary victims (powerless/trapped) — locked in security dilemma; cannot exit without military vulnerability; bear extraction costs through military spending diversion and geopolitical subordination
 *   - Rival Regional Powers: Secondary actors (powerful/constrained) — locked in mutual deterrence; genuine coordination function (preventing war) coupled with high military opportunity costs; constrained by domestic politics and mutual mistrust
 *   - Hegemonic Power: Primary beneficiary (institutional/arbitrage) — captures geopolitical influence through arms sales, military base rights, and deterrence subsidies provided by subordinate states; arbitrage options allow exit from direct competition
 *   - Arms Industry: Institutional beneficiary (institutional/arbitrage) — sustained demand for weapons systems, maintenance, modernization; coordination function (technical enablement) coupled with profit extraction
 *   - Peace Coalition: Organized actors (organized/mobile) — UN peacekeeping, arms control advocates, track-two diplomacy; see military balance as temporary problem addressable through institutional reform; lower effective extraction due to exit options
 *   - Cold War Strategic Doctrine: Institutional framework (institutional/arbitrage) — legacy deterrence logic (MAD, balance of power) persists through institutional inertia; high theater, atrophied function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable laws of international anarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_military_balance_stability, 0.58).
domain_priors:suppression_score(regional_military_balance_stability, 0.72).
domain_priors:theater_ratio(regional_military_balance_stability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_military_balance_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_military_balance_stability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(regional_military_balance_stability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_military_balance_stability, tangled_rope).
narrative_ontology:human_readable(regional_military_balance_stability, "Regional Military Balance Stability Constraint").
narrative_ontology:topic_domain(regional_military_balance_stability, "geopolitical/security/military").

domain_priors:requires_active_enforcement(regional_military_balance_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_military_balance_stability, hegemonic_power).
narrative_ontology:constraint_beneficiary(regional_military_balance_stability, arms_industry).
narrative_ontology:constraint_victim(regional_military_balance_stability, subordinate_states).
narrative_ontology:constraint_victim(regional_military_balance_stability, civilian_populations).
narrative_ontology:constraint_victim(regional_military_balance_stability, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE STATE (SNARE) — Trapped in an arms race by security dilemma logic. Cannot exit without military vulnerability; cannot remain without escalating extraction. High suppression (military necessity rhetoric), high extraction (diversion of resources from civilian needs), minimal coordination benefit. The state perceives the constraint as immutable law of international survival.
constraint_indexing:constraint_classification(regional_military_balance_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RIVAL REGIONAL POWER (TANGLED ROPE) — Two dominant powers locked in mutual deterrence. Genuine coordination function (war prevention) coupled with asymmetric extraction (military expenditure imposes opportunity costs). Both benefit from stability (avoiding war) but bear costs of permanent military readiness. Constrained exit: can negotiate but faces domestic political pressure and mistrust. Effective extraction is substantial but not maximal because mutual destruction creates symmetry.
constraint_indexing:constraint_classification(regional_military_balance_stability, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEGEMONIC POWER (ROPE) — Primary beneficiary. The constraint enables hegemonic stability through arms sales, military-industrial leverage, and geopolitical influence over dependent states. Arbitrage options: can exit arms competition by maintaining technological superiority; can arbitrage between competing regional powers. Experiences the constraint as coordination mechanism that subsidizes hegemony. Extraction runs toward this agent.
constraint_indexing:constraint_classification(regional_military_balance_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ARMS INDUSTRY (ROPE) — Institutional beneficiary. Military balance stability creates sustained demand for weapons systems, maintenance, modernization, and training. Full arbitrage: can sell to both sides, lobby governments, and influence defense policy. Coordination function: arms industry provides technical enablement for deterrence infrastructure. Theater is high (defense doctrines, technical justifications) but coordination is real (weapons must function). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(regional_military_balance_stability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PEACE COALITION / NGO ACTORS (SCAFFOLD) — Organized agents (UN peacekeeping, arms control advocates, track-two diplomacy networks) see the military balance as a temporary coordination failure addressable through institutional reform. Low effective extraction because these agents have exit options (shuttle diplomacy, alternative negotiation forums) and perceive sunset logic: non-proliferation treaties, confidence-building measures, verification regimes are building alternative stability pathways. Theater is moderate (diplomatic ritual); coordination function is real (conflict prevention through dialogue).
constraint_indexing:constraint_classification(regional_military_balance_stability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR STRATEGIC DOCTRINE (PITON) — Legacy institutional framework. Deterrence through mutual assured destruction (MAD) and balance-of-power logic persist in military doctrine despite erosion of the bipolar context that justified them. Theater is high: strategic reviews still invoke 'stability' and 'balance' even as the underlying threat environment has shifted to asymmetric, cyber, and non-state challenges. The doctrine continues through institutional inertia — replacement frameworks have not fully crystallized. Extraction is moderate but function is atrophied.
constraint_indexing:constraint_classification(regional_military_balance_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, the security dilemma appears as an immutable feature of anarchic international systems: when no higher authority can enforce agreements, each state's rational self-defense creates incentive structures that produce arms races regardless of actors' intentions. This perspective sees the military balance as a law of international anarchy. However, the structural data contradicts the mountain classification: identifiable beneficiaries (hegemonic power, arms industry) and victims (subordinate states) exist; institutional arrangements (NATO, bilateral military bases, arms export regimes) actively shape who benefits. The engine will identify this as a false summit: naturalization of contingent geopolitical arrangements.
constraint_indexing:constraint_classification(regional_military_balance_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_military_balance_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_military_balance_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_military_balance_stability, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_military_balance_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_military_balance_stability, TR),
    TR >= 0.70.

:- end_tests(regional_military_balance_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting compound extraction. Base security dilemma logic creates genuine coordination problem (preventing war), reducing extractiveness from hypothetical maximum. But hegemonic power, arms industry, and domestic military institutions actively shape the constraint's boundaries and beneficiary structure — extractiveness rises above pure coordination floor because institutional arrangements concentrate benefits on hegemonic and military-industrial actors while dispersing costs to subordinate states. The 20-year trajectory (0.38 → 0.58) models extraction accumulation: new weapons systems, expanded military bases, and arms control regimes that freeze power asymmetries add extraction layers to the original coordination problem. Suppression (0.72): High and rising. Subordinate states face material barriers to exit (military vulnerability if unilateral disarmament), rhetorical barriers (security dilemma logic presented as immutable necessity), and institutional barriers (alliance dependencies, arms embargo compliance). Military procurement systems have institutional momentum — domestic constituencies (defense contractors, military personnel, nationalist constituencies) actively suppress alternatives. Theater ratio (0.68): High and rising. Strategic defense reviews invoke 'balance' and 'stability' to justify expenditures even as the threat environment shifts to asymmetric and cyber domains that traditional balance-of-power logic does not address. Performative content includes declaratory policy (strategic ambiguity, deterrence rhetoric), military exercises designed for signaling rather than operational requirement, and arms control negotiations that formalize rather than resolve competition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence — all six DR types are legitimate readings of the same structural phenomenon. The subordinate state sees pure extraction (snare) because exit options are blocked and benefits are invisible. The rival regional power sees mixed coordination-extraction (tangled rope) because deterrence genuinely prevents war but costs are substantial and non-negotiable. The hegemonic power sees coordination (rope) because the constraint enables geopolitical leverage at minimal cost — subordinate states bear the military burden. The arms industry sees coordination (rope) because the constraint guarantees demand for products and services. The peace coalition sees a temporary problem with institutional solutions (scaffold) — non-proliferation treaties, regional confidence-building measures, and multilateral institutions can address stability without military competition. The legacy strategic doctrine sees itself as degraded (piton) — it persists through inertia and institutional opposition to alternatives, not because it effectively addresses contemporary security challenges. The civilizational analytical observer risks seeing immutable natural law (mountain) — security dilemma as inevitable feature of anarchy — but the structural data reveals this as false summit: the constraint is maintained by specific institutional choices (alliance structures, military procurement systems, arms export regimes) that could be reformed or replaced.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for each perspective are derived from the agent's structural position relative to the military balance constraint. Subordinate states with trapped exit experience maximum d (≈0.95), experiencing full extraction flow away from them. Rival regional powers with constrained exit and mixed beneficiary/victim status experience moderate d (≈0.52), producing tangled rope. Hegemonic power with arbitrage options and clear beneficiary status experiences low d (≈0.08), producing negative effective extraction (the constraint subsidizes this agent). Arms industry with institutional arbitrage and clear beneficiary status experiences low d (≈0.12). Peace coalition with mobile exit options experience moderate d (≈0.55) but are organized and external to the constraint structure, producing scaffold. Cold War doctrine with arbitrage (can adjust) experiences moderate d but with high theater producing piton. Analytical observer with analytical exit sees full structure at d ≈0.72 but risks naturalizing the observed structure as inevitable rather than contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy through perspectival pluralism. The mandatrophy question 'Is this coordination or extraction?' has no single answer because different structural positions produce different answers. The subordinate state (powerless, trapped) has maximal extraction experience. The hegemonic power (institutional, arbitrage) has maximal coordination experience. Both are structurally correct from their positions. The mandatrophy is not 'which classification is true?' but 'which structural positions are represented in this system?' The false summit (analytical observer seeing mountain/natural law) reveals the naturalizing move: the constraint persists because institutional arrangements make it seem inevitable, not because it is logically inevitable. Non-aligned movement revival, multilateral arms control frameworks, and economic integration architectures could replace military balance, but institutional actors (hegemonic power, military-industrial complex, legacy strategic doctrines) have incentives to maintain the present constraint. The presheaf over the full observation site IS the answer: the constraint is tangled rope at the core (rival powers with mixed coordination-extraction), rope for hegemons and arms industry (net beneficiaries), snare for subordinate states (net victims), scaffold for peace coalitions (exit possible through institutional reform), and piton for legacy doctrines (function atrophied, maintenance through inertia). The false summit naturalizes this entire structure as 'security dilemma logic' when it is actually a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_dilemma_inevitability,
    'Is the security dilemma an immutable feature of anarchic systems or a contingent outcome of specific institutional arrangements?',
    'Historical comparison of multipolar systems with different institutional configurations (Concert of Europe, League of Nations, UN-backed order, non-aligned movement); identification of cases where security dilemma logic was successfully decoupled from military competition',
    'If immutable: mountain classification justified; regional military balance is an inherent constraint on international politics. If contingent: false summit confirmed; the constraint is maintained by institutional choices (military procurement, alliance structures, arms embargoes) rather than by system-level logic. This determines whether exit from the constraint is theoretically possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_dilemma_inevitability, conceptual, 'Whether security dilemma is immutable or contingent').

omega_variable(
    hegemonic_stability_verification,
    'Does hegemonic power actually reduce regional military competition, or does it shift competition from arms races to arms control negotiations and sanctions evasion?',
    'Empirical comparison: regional military expenditure growth under hegemonic vs multipolar systems; measurement of implicit subsidies hegemons provide to client states; tracking of parallel arms markets and sanctions circumvention during periods of hegemonic enforcement',
    'If hegemonic reduction is real: rope classification for hegemonic power is correct — coordination function reduces extraction. If extraction merely shifts form: tangled rope or snare classification more accurate — coordination is theater masking deeper military-industrial extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hegemonic_stability_verification, empirical, 'Whether hegemonic power genuinely reduces regional military competition').

omega_variable(
    subordinate_state_coalition_possibility,
    'Can subordinate states coordinate collective defection from the military balance constraint through multilateral arms control, non-aligned movement revival, or alternative security architectures?',
    'Analysis of historical non-aligned movement, ASEAN centrality doctrine, African Union conflict prevention mechanisms; identification of structural barriers (domestic military institutions, U.S. security guarantees, arms dependency) vs contingent political choices blocking coalition formation',
    'If coalition is structurally impossible: snare classification is accurate; subordinate states face true powerlessness. If coalition is politically blocked: organized power could emerge; powerless becomes organized, shifting classification to tangled rope or scaffold perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinate_state_coalition_possibility, empirical, 'Whether subordinate state coalition defection from military balance is possible').

omega_variable(
    arms_control_treaty_enforcement,
    'Do arms control treaties (NPT, CTBT, MTCR) function as genuine coordination mechanisms or as hegemonic tools to freeze regional power asymmetries?',
    'Systematic review of treaty enforcement patterns: which violations trigger sanctions vs which are overlooked; correlation between treaty signatory status and geopolitical alignment; analysis of verification regime asymmetry (inspections of non-aligned vs allied states)',
    'If genuine coordination: rope classification for treaty systems is correct. If hegemonic asymmetry: arms control treaties are snare instruments masquerading as rope — victims perceive them as extraction mechanisms that legitimize hegemonic nuclear monopoly while denying subordinate states deterrence capability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arms_control_treaty_enforcement, empirical, 'Whether arms control treaties function as coordination or hegemonic extraction').

omega_variable(
    alternative_deterrence_architectures,
    'Are non-military deterrence mechanisms (economic integration, mutual development, institutional embedding) capable of replacing military balance as the stability mechanism?',
    'Case studies of successful conflict prevention without military parity (Nordic model, MERCOSUR, ASEAN plus mechanisms); identification of preconditions for non-military deterrence stability; measurement of breakdown triggers when economic integration fails (Brexit, pre-WWI trade networks)',
    'If viable: scaffold classification is correct — military balance is temporary institutional form replaceable by higher-trust alternatives. If preconditions are rare or fragile: military balance persists as necessary compromise; tangled rope or snare more accurate depending on agent perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_deterrence_architectures, empirical, 'Viability of alternative deterrence architectures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_military_balance_stability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rmbs_tr_t0, regional_military_balance_stability, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rmbs_tr_t10, regional_military_balance_stability, theater_ratio, 10, 0.62).
narrative_ontology:measurement(rmbs_tr_t20, regional_military_balance_stability, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(rmbs_be_t0, regional_military_balance_stability, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rmbs_be_t10, regional_military_balance_stability, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rmbs_be_t20, regional_military_balance_stability, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rmbs_su_t0, regional_military_balance_stability, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(rmbs_su_t10, regional_military_balance_stability, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(rmbs_su_t20, regional_military_balance_stability, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_military_balance_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regional_military_balance_stability, 0.25).
narrative_ontology:affects_constraint(regional_military_balance_stability, hegemonic_power_sustainability).
narrative_ontology:affects_constraint(regional_military_balance_stability, arms_industry_profit_extraction).
narrative_ontology:affects_constraint(regional_military_balance_stability, subordinate_state_development_capacity).
narrative_ontology:affects_constraint(regional_military_balance_stability, non_proliferation_treaty_enforcement).
narrative_ontology:affects_constraint(regional_military_balance_stability, alliance_dependency_lock).

% DUAL FORMULATION NOTE:
% The regional military balance constraint decomposes into multiple structurally distinct constraints. The pure security dilemma coordination problem (preventing war through deterrence) is distinct from the institutional arrangements that concentrate benefits on hegemonic actors; from the arms industry profit mechanisms; and from the victim structures of civilian populations. Each decomposition has different ε values. The security coordination core has low ε (genuine mutual benefit from peace); the hegemonic benefit structure has moderate ε (unequal distribution of coordination gains); the arms industry extraction has high ε (minimal coordination function, pure profit capture); the victim structures (subordinate states, civilian populations) have high ε (pure cost bearing with minimal benefit). This story addresses the aggregate constraint; sibling stories address specific decompositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_military_balance_stability, institutional, 0.08).
constraint_indexing:directionality_override(regional_military_balance_stability, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
