% ============================================================================
% CONSTRAINT STORY: pla_loyalty_purge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pla_loyalty_purge, []).

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
 *   constraint_id: pla_loyalty_purge
 *   human_readable: PLA Loyalty Purge Mechanism
 *   domain: political/military_coercion
 *
 * SUMMARY:
 *   The People's Liberation Army loyalty purge under Xi Jinping represents a
 *   structural mechanism of coercive organizational consolidation within
 *   China's military institution. Since Xi assumed leadership of the Central
 *   Military Commission in 2012, the PLA has undergone sustained removal of
 *   senior officers and regional commanders, officially framed as
 *   anti-corruption campaigns but structurally functioning as a consolidation
 *   of factional control. The constraint exhibits both genuine coordination
 *   features (aligning military command hierarchy with civilian party
 *   authority) and pure extraction features (removing power competitors,
 *   enforcing ideological compliance, establishing surveillance-based loyalty
 *   mechanisms). The purge operates through multiple enforcement channels:
 *   formal anti-corruption investigations, loyalty denunciations, performance
 *   evaluations, and career termination. Officers face trapped exit options:
 *   resignation invites investigation, defection constitutes national
 *   betrayal, internal opposition triggers documentation as disloyalty. The
 *   mechanism relies on suppression of alternatives and institutional
 *   surveillance to maintain compliance. The theater ratio (0.55) reflects
 *   that formal bureaucratic procedures (performance reviews, corruption
 *   charges, promotion rituals) provide legitimating cover for what is
 *   substantially an arbitrary loyalty enforcement mechanism.
 *
 * KEY AGENTS:
 *   - Xi Jinping faction: Primary beneficiary (institutional/arbitrage) — consolidates power by removing military competitors; controls appointment and removal mechanisms
 *   - Officer corps (targeted): Primary victim (powerless/trapped) — face career termination, reputation destruction, and investigation risk; cannot exit or resist without facing career consequences
 *   - Regional military commanders: Secondary victim (organized/constrained) — balance operational autonomy against loyalty pressure; face resource centralization and command authority constraints
 *   - Central Military Commission apparatus: Institutional enforcer (institutional/immediate) — implements purges, manages denunciations, maintains surveillance systems
 *   - Military bureaucracy: Institutional theater provider (institutional/arbitrage) — maintains formal procedures that legitimize purges; provides plausible deniability
 *   - Analytical observer: Structural analyst (analytical/analytical) — identifies both genuine coordination and extraction functions operating simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pla_loyalty_purge, 0.68).
domain_priors:suppression_score(pla_loyalty_purge, 0.78).
domain_priors:theater_ratio(pla_loyalty_purge, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pla_loyalty_purge, extractiveness, 0.68).
narrative_ontology:constraint_metric(pla_loyalty_purge, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(pla_loyalty_purge, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pla_loyalty_purge, snare).
narrative_ontology:human_readable(pla_loyalty_purge, "PLA Loyalty Purge Mechanism").
narrative_ontology:topic_domain(pla_loyalty_purge, "political/military_coercion").

domain_priors:requires_active_enforcement(pla_loyalty_purge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pla_loyalty_purge, xi_faction_loyalists).
narrative_ontology:constraint_victim(pla_loyalty_purge, pla_officer_corps).
narrative_ontology:constraint_victim(pla_loyalty_purge, regional_military_commanders).
narrative_ontology:constraint_victim(pla_loyalty_purge, institutional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TARGETED OFFICER (SNARE) — Military officers subject to purges face maximum extraction. Exit is structurally trapped: resignation invites investigation, defection is national betrayal, internal opposition is documented as disloyalty. The purge mechanism relies on surveillance, denunciation, and career termination as enforcement tools. No genuine alternatives exist.
constraint_indexing:constraint_classification(pla_loyalty_purge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL COMMAND STRUCTURE (TANGLED ROPE) — Regional military leaders experience both coordination (clear chain of command) and extraction (loyalty pressure, resource centralization, threat of purge). They benefit from institutional stability but at cost of autonomy. Exit is constrained — resignation is possible but career-ending; internal restructuring is possible within limits.
constraint_indexing:constraint_classification(pla_loyalty_purge, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: XI FACTION LOYALISTS (ROPE) — Core loyalists experience the purge as pure coordination: identifying threats, consolidating command, clarifying hierarchy. They benefit from removing competitors and strengthening institutional alignment. Exit options are favorable — they can arbitrage between civilian and military authority, between competing factions, between central and local power bases.
constraint_indexing:constraint_classification(pla_loyalty_purge, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REFORM ADVOCATES (SCAFFOLD) — Some reformers view the purge as a temporary mechanism for consolidating civilian party control over a historically autonomous military institution. The extraction (surveillance, fear) is tolerated as temporary, with expectation that once unified command is achieved, fear-based enforcement will decline. Sunset logic: as meritocratic promotion norms replace purge-based discipline, effective extraction should decrease. Sunset timeline: 10-15 years if institutional norm-setting succeeds.
constraint_indexing:constraint_classification(pla_loyalty_purge, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MILITARY BUREAUCRACY (PITON) — The formal structure of military discipline, hierarchy, and performance evaluation persists and provides theater for purges. Anti-corruption campaigns provide official justification; performance metrics justify removals; promotion rituals maintain institutional legitimacy. The bureaucracy itself has atrophied as a constraint on arbitrary action — formal procedures are followed but are substantially performative, enabling purges that would be impossible under genuine rule-of-law constraints.
constraint_indexing:constraint_classification(pla_loyalty_purge, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global and long-term view, the purge serves two structural functions: (1) genuine coordination to align military institution with civilian party control and (2) extraction to consolidate factional power and suppress alternative power centers. Both functions are real and structural, not reducible to one or the other. The constraint exhibits both Rope features (coordination of military hierarchy) and Snare features (suppression of dissent and removal of competitors).
constraint_indexing:constraint_classification(pla_loyalty_purge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pla_loyalty_purge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pla_loyalty_purge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pla_loyalty_purge, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pla_loyalty_purge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pla_loyalty_purge, TR),
    TR >= 0.70.

:- end_tests(pla_loyalty_purge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The purge extracts loyalty compliance from the officer corps through threat and career termination. Benefits accrue to the Xi faction (consolidated control, removal of competitors) while costs fall on targeted officers and the institutional autonomy of the military structure itself. The measurement trajectory (0.45 → 0.62 → 0.68) shows rising extraction intensity over the purge interval, indicating escalation rather than stabilization. Suppression (0.78): Very high. Officers have minimal alternatives: internal options (resistance, denunciation, lateral movement) trigger investigation; external options (resignation, emigration) trigger investigation and career destruction; strategic silence is the dominant strategy but provides no protection. The suppression arises from comprehensive surveillance, institutional control over advancement, and the life-or-death stakes of military careers. Theater ratio (0.55): Moderate. The purge is not pure performance — genuine institutional restructuring occurs and real officers are removed — but the formal mechanisms (anti-corruption charges, performance metrics) provide substantial theater. Charges may be legitimate or fabricated; the distinction is often unknowable to observers. The theater has remained stable because it serves a legitimating function that cannot be abandoned without revealing the arbitrary nature of the enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The targeted officer sees pure extraction (Snare) — their exit is trapped and costs are total. The regional command sees mixed coordination and extraction (Tangled Rope) — they benefit from unified command but at cost of autonomy. The Xi faction sees pure coordination (Rope) — the purge solves the problem of military alignment with party authority. The institutional reformer sees temporary extraction with sunset (Scaffold) — once unified command is established, fear-based discipline can decline. The military bureaucracy sees its own degraded legitimacy (Piton) — formal procedures persist but are increasingly performative. The analytical observer sees genuine Tangled Rope — both coordination and extraction are real structural features that cannot be separated. This perspectival gap is irreducible: it reflects genuine differences in structural position, not measurement error or perspective bias.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by structural position. Targeted officers (powerless/trapped) have d ≈ 0.95, experiencing maximum extraction through the sigmoid function. Regional commanders (organized/constrained) have d ≈ 0.55-0.65, experiencing mixed coordination and extraction. Xi faction loyalists (institutional/arbitrage) have d ≈ 0.05-0.15, experiencing coordination benefits with minimal extraction cost. The purge mechanism relies on high suppression (0.78) to maintain extraction despite the costs it imposes — suppression is not scaled by d, it is a structural feature of the constraint that applies equally across positions. The beneficiary/victim declarations reflect real structural asymmetry: the Xi faction benefits; targeted officers, regional command autonomy, and military institution integrity are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH TANGLED ROPE AUTHENTICATION: The constraint resolves the mandatrophy by acknowledging that the purge serves TWO structurally real and inseparable functions: (1) genuine coordination to establish civilian party control over the military (Rope function) and (2) extraction to consolidate factional power and suppress alternative power centers (Snare function). Neither can be eliminated without destroying the other. The Xi faction cannot consolidate party control without removing competing power centers. The officer corps cannot resist without threatening party-military relations. The analytical observer cannot separate the legitimate institutional function from the factional extraction mechanism — they are structurally coupled in the purge mechanism itself. Tangled Rope classification is the correct analytical category because both functions are real and both extractiveness and active enforcement are required by the structural design. The constraint cannot be understood as 'really just' coordination with incidental extraction side effects, nor as 'really just' extraction dressed up in coordination language — it is genuinely hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_corruption_versus_purge_boundary,
    'How much of the officer removal is driven by genuine corruption investigation versus factional loyalty pressure?',
    'Comparative analysis of corruption charges: conviction rates for purged officers vs non-purged officers; correlation between purge waves and factional threat perception; consistency of charges across regions and institutional levels',
    'If primarily corruption-driven: reclassifies as Scaffold with sunset logic (corruption-driven reforms can eventually normalize). If primarily loyalty-driven: confirms Snare classification. If mixed: confirms Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_corruption_versus_purge_boundary, empirical, 'Boundary between legitimate anti-corruption and factional purge').

omega_variable(
    institutional_norm_recovery_timeline,
    'After centralization is achieved, will fear-based enforcement decline or become permanent feature of military hierarchy?',
    'Historical comparison to Deng-era military reforms, tracking of removal rates over next 15 years, correlation between institutional stability and purge frequency, analysis of successor planning under unified command',
    'If extraction declines as promised: Scaffold classification confirmed, sunset is real, constraint transitions to Rope. If extraction remains high: Snare persists, purge becomes institutionalized coercion, constraint is mislabeled as temporary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_norm_recovery_timeline, preference, 'Whether purge-based discipline can transition to institutional norms').

omega_variable(
    officer_corps_exit_elasticity,
    'As purge risk increases, do capable officers seek exit (emigration, early retirement, sector change), creating brain drain that forces higher extraction to maintain compliance?',
    'Tracking of PLA officer emigration rates; analysis of early retirement requests; correlation between purge intensity and retirements; comparison to exit rates in professional militaries',
    'If exit elasticity is high: extractiveness becomes unstable (purges cause exits, exits require higher extraction to maintain compliance, higher extraction causes more exits). If exit is trapped: Snare persists stably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_corps_exit_elasticity, empirical, 'Whether officer exit flows create feedback loop with extraction intensity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pla_loyalty_purge, 2012, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pla_tr_t0, pla_loyalty_purge, theater_ratio, 0, 0.5).
narrative_ontology:measurement(pla_tr_t3, pla_loyalty_purge, theater_ratio, 3, 0.52).
narrative_ontology:measurement(pla_tr_t6, pla_loyalty_purge, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(pla_be_t0, pla_loyalty_purge, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pla_be_t3, pla_loyalty_purge, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(pla_be_t6, pla_loyalty_purge, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pla_loyalty_purge, enforcement_mechanism).
narrative_ontology:affects_constraint(pla_loyalty_purge, chinese_civil_military_relations).
narrative_ontology:affects_constraint(pla_loyalty_purge, factional_power_consolidation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pla_loyalty_purge, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
