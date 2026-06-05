% ============================================================================
% CONSTRAINT STORY: xi_mao_ideological_centralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_xi_mao_ideological_centralization, []).

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
 *   constraint_id: xi_mao_ideological_centralization
 *   human_readable: Ideological Centralization and the Leadership Core
 *   domain: political/institutional_power
 *
 * SUMMARY:
 *   Ideological centralization represents a structural constraint on power
 *   distribution within a party-state system. The constraint describes how
 *   centralized control over orthodoxy — who defines correct ideology and
 *   what constitutes deviation — becomes a mechanism to concentrate authority
 *   in the leadership core and extract institutional autonomy from
 *   intermediate power structures. This constraint must be distinguished from
 *   genuine coordination (which also requires some alignment on core
 *   principles) by examining whether the system extracts from those it
 *   nominally coordinates with, whether exit from the system is blocked, and
 *   whether internal succession norms are dismantled. The measurement
 *   trajectory shows increasing extractiveness (0.35 → 0.68) and increasing
 *   theater ratio (0.40 → 0.65), suggesting that the constraint has degraded
 *   from a mixed system (some genuine coordination, some extraction) toward
 *   pure extraction with more performative elements. The piton perspective
 *   (the degradation of the Deng-era succession norms) is central: this
 *   constraint succeeds precisely by hollowing out an earlier constraint
 *   system that genuinely distributed power.
 *
 * KEY AGENTS:
 *   - Central Leadership Core: Primary beneficiary (institutional/arbitrage) — consolidates power through ideological authority, sets orthodoxy rules, can reshape ideology to maintain control
 *   - Cadre and Party Functionary: Primary victim (powerless/trapped) — career dependent on ideological alignment, no exit from party system, subject to arbitrary orthodoxy interpretation
 *   - Mid-Tier Official: Secondary victim (moderate/constrained) — faces ideological enforcement through merit system degradation, nominally participates in governance but constrained by orthodoxy requirement
 *   - Regional Power Base: Secondary victim (organized/constrained) — institutional actor nominally participating in collective forums but extraction occurs through ideological enforcement of central orthodoxy
 *   - Formal Succession System: Institutional actor (institutional/arbitrage) — the Deng-era retirement and term-limit norms that once constrained leadership power are now degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as contingent institutional arrangement extracting autonomy, not as immutable coordination necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(xi_mao_ideological_centralization, 0.68).
domain_priors:suppression_score(xi_mao_ideological_centralization, 0.75).
domain_priors:theater_ratio(xi_mao_ideological_centralization, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, extractiveness, 0.68).
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(xi_mao_ideological_centralization, snare).
narrative_ontology:human_readable(xi_mao_ideological_centralization, "Ideological Centralization and the Leadership Core").
narrative_ontology:topic_domain(xi_mao_ideological_centralization, "political/institutional_power").

domain_priors:requires_active_enforcement(xi_mao_ideological_centralization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(xi_mao_ideological_centralization, central_leadership_core).
narrative_ontology:constraint_victim(xi_mao_ideological_centralization, institutional_succession_norms).
narrative_ontology:constraint_victim(xi_mao_ideological_centralization, cadre_circulation_system).
narrative_ontology:constraint_victim(xi_mao_ideological_centralization, party_institutional_autonomy).
narrative_ontology:constraint_victim(xi_mao_ideological_centralization, internal_party_factions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CADRE AND PARTY FUNCTIONARY (SNARE) — Trapped within the party-state system with no exit option. Career advancement depends entirely on ideological alignment with the leadership core. Cannot exit the system or challenge orthodoxy without career destruction. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.66 (snare threshold).
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER OFFICIAL (SNARE) — Constrained by resource dependencies and hierarchical control. Ideological deviation creates immediate career jeopardy. Has marginally more exit capacity than lower cadres but still trapped within the system. d≈0.80, f(d)≈1.18, σ=1.0 → χ≈0.60 (snare territory).
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL POWER BASE (TANGLED ROPE) — Organized institutional actor (provincial party apparatus) that nominally participates in coordination through collective decision-making forums (Politburo, Central Committee), but faces extraction through ideological enforcement and merit system degradation. Constrained because defection from the party system is impossible but has some institutional leverage. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.51 (tangled rope midpoint).
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL LEADERSHIP CORE (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: ideological centralization consolidates command authority and enables rapid decision-making. Arbitrage capacity (ability to define and redefine ideology) means the core sets the rules. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07 (net beneficiary, rope classification).
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL SUCCESSION SYSTEM (PITON) — The Deng-era retirement norms (term limits, mandatory succession, age-based advancement) persist as institutional theater (theater_ratio=0.65) while real power dynamics are driven by ideological alignment. The formal system still exists (Politburo Standing Committee, collective decision procedures) but is performative — actual succession is now determined by whether an successor maintains ideological orthodoxy to the core. This represents degradation of an earlier constraint (the succession norm system) that once functioned to distribute power. d≈0.05, f(d)≈-0.10, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a long-term analytical perspective, the constraint represents extraction from the entire institutional party-state apparatus. Ideological centralization extracts institutional autonomy, meritocratic advancement, and internal deliberation norms. The system that once distributed power through cadre circulation now concentrates it through ideology. From the civilizational view, this is not a mountain (immutable law) but a contingent institutional arrangement designed to maximize extractive authority. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.68 (snare).
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(xi_mao_ideological_centralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(xi_mao_ideological_centralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(xi_mao_ideological_centralization, TR),
    TR >= 0.70.

:- end_tests(xi_mao_ideological_centralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.68): High. The constraint extracts institutional autonomy from cadres, regional power bases, and the formal succession system. The central core sets the rules of ideological orthodoxy (beneficiary + arbitrage), while subordinate structures must conform without the capacity to challenge or negotiate (victims + trapped/constrained). The extraction increases over time as ideological enforcement becomes more stringent and as tolerance for internal debate narrows. Suppression (0.75): Very high. Multiple mechanisms suppress alternatives: (1) career penalties for ideological deviation, (2) lack of formal channels for factional expression or succession negotiation, (3) control over information and interpretation of past decisions, (4) inability to exit the party-state without forfeiting status and access. Theater Ratio (0.65): Moderate-high and increasing. Formal mechanisms (Politburo Standing Committee, Central Committee, collective decision procedures) perform governance functions but increasingly lack real decision-making power — actual authority is concentrated in those who define ideology. The formal system's theater has increased as the mechanisms persist but become less real. This trajectory signals piton dynamics: a constraint system that once genuinely distributed power is becoming performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The central leadership core experiences it as pure coordination (Rope) — ideological alignment enables unified action and rapid decision-making. The analytical observer sees it as pure extraction (Snare) — the system extracts institutional autonomy. The cadre and lower officials see it as a snare trapping them within the system. The regional power base sees tangled rope — nominally participating in coordination but subject to extraction through ideological enforcement. The formal succession system is treated as piton — the Deng-era norms that once constrained power persist as theater while real succession depends on ideological loyalty. The perspectival gap reveals that what the core calls 'coordination' the victims call 'extraction,' because the core has unilateral power to set the rules and the victims have no exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Central Leadership Core: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary (negative effective extraction). Cadre/Functionary: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction coefficient. Mid-Tier Official: Victim + constrained → d≈0.80, f(d)≈1.18. High extraction, somewhat constrained by having minimal institutional leverage. Regional Power Base: Victim + constrained + organized → d≈0.65, f(d)≈1.00. The organizational capacity moderates directionality somewhat, but constraint remains binding because exit is impossible (cannot leave the party-state without loss of all status). Formal Succession System: Institutional + arbitrage → d≈0.05, f(d)≈-0.10 as a nominal entity, but the piton classification reflects that the system has been hollowed — it exists formally but lacks real function. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. External view sees the entire apparatus as victimized by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: This constraint exhibits all three snare gates. (1) Extractiveness ≥ 0.46: measured at 0.68 — clearly met. (2) Suppression ≥ 0.60: measured at 0.75 — clearly met. (3) Effective extraction χ ≥ 0.66: computed from the analytical perspective as χ = 0.68 × 1.15 × 1.0 = 0.78 (exceeds snare threshold). The mandatrophy avoidance is clear: the snare classification is not masquerading as coordination (rope) because the core extracts from subordinates while claiming they coordinate. The piton perspective (degradation of succession norms) confirms that this is not a natural law but a contingent institutional arrangement. The increasing theater_ratio (0.40 → 0.65) and extractiveness (0.35 → 0.68) over the measurement interval show that the constraint has intensified: what began with some genuine coordination functions has evolved toward pure extraction with performative governance mechanisms. The tangled rope perspective (regional power bases) shows the transition point — these institutional actors once had more real participation; as ideology becomes the sole arbiter of authority, they shift toward pure victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideological_consistency_definition,
    'What counts as ideological orthodoxy, and who determines the boundary between acceptable interpretation and deviation?',
    'Historical documentation of disciplinary actions for ''ideological errors''; analysis of party communication documents and their interpretation shifts over time; interviews with cadre regarding ideological ambiguity and enforcement discretion',
    'If orthodoxy is strictly defined: cadres have measurable boundaries for compliance (extractiveness lower, suppression lower). If orthodoxy is fluid and determined by core preferences: extractiveness becomes unlimited (χ approaches maximum).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_consistency_definition, conceptual, 'Definition and boundary of ideological orthodoxy').

omega_variable(
    institutional_succession_alternative_pathways,
    'Could alternative succession mechanisms (technocratic meritocracy, electoral competition, factional negotiation) constrain leadership extraction and restore institutional autonomy?',
    'Comparative analysis of party-states with different succession mechanisms (Taiwan under DPP, Vietnam''s collective leadership, Mexico''s PRI rotation); counterfactual modeling of Chinese institutions under term-limited succession norms',
    'If alternatives are structurally viable: current centralization is a choice, not inevitability (snare confirmed). If alternatives fail: current system emerges as necessary coordination mechanism (rope reclassification possible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_succession_alternative_pathways, empirical, 'Whether alternative succession mechanisms could work in context').

omega_variable(
    cadre_circulation_genuine_replacement,
    'Do rotations within the formal system represent genuine power transfers or procedural theater hiding continued influence?',
    'Analysis of retired officials'' influence networks post-retirement; comparison of policy changes before/after official transitions; documentation of informal power structures (standing committees, informal groups, family networks) that persist through formal transitions',
    'If rotation is genuine: snare classification confirmed (formal rules are theater masking real power). If rotation is complete: some aspects reclassify as rope (system genuinely distributes authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cadre_circulation_genuine_replacement, empirical, 'Whether cadre rotations represent genuine power transfers').

omega_variable(
    party_unification_versus_factional_suppression,
    'Is ideological centralization a solution to intra-party factional conflict or a suppression mechanism for natural factional competition?',
    'Historical analysis of pre-centralization factional dynamics; comparative study of party-states with factional tolerance vs suppression; modeling of institutional stability under different factional regimes',
    'If factional suppression is necessary: centralization appears as coordination (rope or tangled rope). If factional competition could be channeled: suppression is extractive (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_unification_versus_factional_suppression, empirical, 'Ideological centralization as factional suppression vs conflict resolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(xi_mao_ideological_centralization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(xim_tr_t0, xi_mao_ideological_centralization, theater_ratio, 0, 0.4).
narrative_ontology:measurement(xim_tr_t5, xi_mao_ideological_centralization, theater_ratio, 5, 0.52).
narrative_ontology:measurement(xim_tr_t10, xi_mao_ideological_centralization, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(xim_be_t0, xi_mao_ideological_centralization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(xim_be_t5, xi_mao_ideological_centralization, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(xim_be_t10, xi_mao_ideological_centralization, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(xi_mao_ideological_centralization, enforcement_mechanism).
narrative_ontology:affects_constraint(xi_mao_ideological_centralization, cadre_merit_system_degradation).
narrative_ontology:affects_constraint(xi_mao_ideological_centralization, collective_leadership_norms_erosion).
narrative_ontology:affects_constraint(xi_mao_ideological_centralization, factional_suppression_mechanism).

% DUAL FORMULATION NOTE:
% Ideological centralization is downstream of (and reinforces) cadre merit system degradation and erosion of collective leadership norms. This constraint and its network neighbors form an institutional cluster where centralization of ideology is the mechanism that achieves extraction across multiple dimensions (succession, advancement, deliberation). Each neighboring constraint has its own ε value reflecting specific empirical claims, but all three are structurally coupled through the shared mechanism of ideological orthodoxy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(xi_mao_ideological_centralization, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
