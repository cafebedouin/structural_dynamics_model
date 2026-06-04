% ============================================================================
% CONSTRAINT STORY: roman_republican_constitution__crisis_machinery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_republican_constitution__crisis_machinery, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: roman_republican_constitution__crisis_machinery
 *   human_readable: Roman Republican Crisis Machinery: Dictatorship and Final Decree
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   The Roman Republic's constitution included its own suspension mechanism:
 *   the dictatorship (dictatura) and the senatus consultum ultimum (final
 *   decree). When the state faced existential threat — invasion, plague,
 *   internal collapse — the Senate could declare a crisis and appoint a
 *   single magistrate with supreme power, temporarily removing the checks
 *   that normally constrained authority: the collegiality of offices, the
 *   veto power of tribunes, the right of appeal to assemblies. The dictator
 *   or consul acting under final decree could issue commands without
 *   consultation, conscript armies without assembly vote, execute citizens
 *   without trial. This machinery was explicitly framed as temporary: the
 *   dictatorship had a term-limit (originally 6 months, later adjusted), and
 *   the final decree was implicitly revoked when the crisis passed. The
 *   mechanism was rationalized as the constitution recognizing its own limit
 *   — that ordinary rules cannot govern the moment when those rules
 *   themselves are threatened. However, the crisis machinery also enabled
 *   extraction. Strongmen could exploit ambiguity about when a crisis 'truly'
 *   ended to extend their power (Sulla, Caesar, Pompey). The suppression of
 *   ordinary checks created space for the strongman to accumulate wealth,
 *   clientage, and legitimacy. The victims were multiple: the tribunes lost
 *   their veto, the assemblies lost their law-making power, the Senate lost
 *   its deliberative role, and the plebs lost their constitutional
 *   protections. This reading of the Republican constitution — the crisis
 *   machinery reading — instantiates one contested kernel. Other readings
 *   emphasize the Twelve Tables' codification (law wrested from priestly
 *   control), the magistracies' collegiality (power parceled into pieces),
 *   the popular assemblies and tribunes' veto (plebeian protection), or the
 *   Senate's auctoritas (permanent authority). The crisis machinery reading
 *   claims that the constitution's deepest principle is its own
 *   suspendability — that the Republic recognized it could not survive
 *   rigidly adhering to its ordinary rules during existential threat.
 *
 * KEY AGENTS:
 *   - The State Apparatus: Collective institutional body (institutional/arbitrage) — benefits from crisis machinery as coordination mechanism for survival; experiences no extraction
 *   - The Plebeian Body: Powerless agents (powerless/trapped) — loses tribunes' veto, assemblies' power, and right of appeal during crisis; bears full extraction
 *   - The Senate: Moderate-to-powerful institutional actor (powerful/constrained) — preserves itself through crisis machinery but loses its auctoritas during the emergency; mixed benefit and cost
 *   - The Strongman Magistrate: Powerful individual with supreme power (powerful/constrained) — appears to benefit but is trapped by term-limit: must return power or face proscription/assassination; bounded extraction
 *   - Republican Institutional Coalition: Organized actors (organized/mobile) — tribunes, magistrates, families maintaining constitutional norms; enforce the sunset and maintain the machinery as temporary
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks seeing crisis machinery as natural law of political order; may naturalize contingent design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_republican_constitution__crisis_machinery, 0.68).
domain_priors:suppression_score(roman_republican_constitution__crisis_machinery, 0.78).
domain_priors:theater_ratio(roman_republican_constitution__crisis_machinery, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_republican_constitution__crisis_machinery, extractiveness, 0.68).
narrative_ontology:constraint_metric(roman_republican_constitution__crisis_machinery, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(roman_republican_constitution__crisis_machinery, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_republican_constitution__crisis_machinery, tangled_rope).
narrative_ontology:human_readable(roman_republican_constitution__crisis_machinery, "Roman Republican Crisis Machinery: Dictatorship and Final Decree").
narrative_ontology:topic_domain(roman_republican_constitution__crisis_machinery, "political/historical/constitutional").

domain_priors:requires_active_enforcement(roman_republican_constitution__crisis_machinery).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(roman_republican_constitution__crisis_machinery, '0292c5f0-9a54-40c6-9a14-c13f14bfbbb7').
narrative_ontology:cs_kernel_codification('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', formalized).
narrative_ontology:cs_authority_grounding('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', lineage).
narrative_ontology:cs_interpretation_layer_present('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7').
narrative_ontology:cs_reading_relation('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', roman_republican_constitution__legal_codification_twelve_tables, influences).
narrative_ontology:cs_reading_relation('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', roman_republican_constitution__magistracies_and_collegiality, forecloses).
narrative_ontology:cs_reading_relation('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', roman_republican_constitution__popular_assemblies_and_tribunate, forecloses).
narrative_ontology:cs_reading_relation('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', roman_republican_constitution__senate_authority, forecloses).
narrative_ontology:cs_axiom('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', foundational, existential_threat_overrides_ordinary_rules).
narrative_ontology:cs_axiom_status(existential_threat_overrides_ordinary_rules, holdable).
narrative_ontology:cs_axiom_grounding('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', existential_threat_overrides_ordinary_rules, deontological).
narrative_ontology:cs_axiom('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', foundational, temporal_centralization_preserves_distributed_order).
narrative_ontology:cs_axiom_status(temporal_centralization_preserves_distributed_order, holdable).
narrative_ontology:cs_axiom_grounding('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', temporal_centralization_preserves_distributed_order, instrumental).
narrative_ontology:cs_reference_frame('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', constitutional_suspension_as_constitutional_principle).
narrative_ontology:cs_drift_state('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', late_republic_crisis_proliferation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('0292c5f0-9a54-40c6-9a14-c13f14bfbbb7', '').
narrative_ontology:cs_kernel_id(roman_republican_constitution__crisis_machinery, roman_republican_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_republican_constitution__crisis_machinery, state_survival_apparatus).
narrative_ontology:constraint_beneficiary(roman_republican_constitution__crisis_machinery, strongman_magistrate).
narrative_ontology:constraint_victim(roman_republican_constitution__crisis_machinery, ordinary_checks_and_vetoes).
narrative_ontology:constraint_victim(roman_republican_constitution__crisis_machinery, plebeian_tribunes).
narrative_ontology:constraint_victim(roman_republican_constitution__crisis_machinery, senate_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBEIAN ASSEMBLIES & TRIBUNATE (SNARE) — During dictatorship or senatus consultum ultimum, the tribunes' sacrosanct veto is suspended, the assemblies cannot pass law, and the plebs have no appeal. Extraction is maximal: survival authority flows entirely to the strongman. The crisis machinery traps the powerless in absolute submission. Their ordinary constitutional protections are removed by the same constitution that granted them.
constraint_indexing:constraint_classification(roman_republican_constitution__crisis_machinery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SENATE AUTHORITY (TANGLED ROPE) — The Senate benefits from crisis machinery because it preserves the state, which preserves the Senate's role. But the Senate also loses its auctoritas: the dictator or consul acting under senatus consultum ultimum can override Senate deliberation. The Senate coordinates the state's survival but also experiences extraction — its authority is temporarily neutered. Mixed benefit and cost.
constraint_indexing:constraint_classification(roman_republican_constitution__crisis_machinery, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE SURVIVAL (ROPE) — From the collective perspective of the state as a coordinating body, the crisis machinery is pure coordination: it solves the collective action problem of rapid, unified response to existential threats. The state experiences no extraction; it experiences coordination. The machinery is self-referential: the constraint that saves the Republic IS the Republic's constitution recognizing its own limit.
constraint_indexing:constraint_classification(roman_republican_constitution__crisis_machinery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STRONGMAN MAGISTRATE (SNARE) — The dictator or consul acting under senatus consultum ultimum appears to have supreme power. But this perspective reveals extraction: the strongman is constitutionally bound to return power at the crisis's end. The term-limit is enforced socially and legally — violating it breaks the entire constitutional order and triggers the victimization of the strongman themselves (e.g., Sulla proscriptions, Caesar's assassination). The machinery extracts from the strongman too: they must accept supreme power and then release it, or face exile, proscription, or death. Extractiveness is unbounded INSIDE the window but bounded by the constitutional sunset.
constraint_indexing:constraint_classification(roman_republican_constitution__crisis_machinery, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN INSTITUTIONAL COALITION (SCAFFOLD) — From the perspective of the institutions that maintain the Republic's constitution — Senate families, magistrates, tribunes, assemblies — the crisis machinery is a temporary coordination mechanism with built-in sunset. It solves the crisis AND it has a term-limit gate. The coalition's effectiveness depends on enforcing the sunset: when the emergency passes, the crisis machinery must be deactivated. This is structurally low-extraction (χ ≤ 0.30) because the beneficiaries (the state, the institutions) have agency to enforce the sunset.
constraint_indexing:constraint_classification(roman_republican_constitution__crisis_machinery, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL / CONSTITUTIONAL NATURAL LAW (MOUNTAIN) — From a civilizational analytical view, the crisis machinery appears as an immutable structural principle: every republic must include its own suspension, or it cannot survive genuine existential threats. The principle appears to be a natural law of political organization: states without crisis machinery cannot defend themselves; states that cannot suspend their ordinary rules cannot defend their rules. This perspective risks naturalizing the contingent institutional design as if it were a law of nature. However, the empirical data shows extractiveness ≥ 0.68 and suppression ≥ 0.78, which contradicts the mountain classification. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(roman_republican_constitution__crisis_machinery, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_republican_constitution__crisis_machinery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_republican_constitution__crisis_machinery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_republican_constitution__crisis_machinery, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_republican_constitution__crisis_machinery, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(roman_republican_constitution__crisis_machinery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but bounded. During the crisis window, the strongman extracts maximum authority — supreme power, immunity from veto and appeal. But the term-limit is real: when the crisis ends, the strongman must return power. The extractiveness value reflects that the window is measurable (not indefinite) and that return is constitutionally and socially enforced. If the term-limit were not enforced, extractiveness would approach 1.0 (true indefinite dictatorship). The current value (0.68) reflects the bounded extraction inside a defined temporal window. Suppression (0.78): Very high during crisis. The tribunes' veto is suspended. The assemblies cannot pass law. Appeals are blocked. The plebs have no exit and no voice. Suppression is the entire mechanism: the crisis machinery works BY suppressing ordinary checks. However, suppression is explicitly temporary — when the crisis ends, the checks return. Outside the window, suppression falls to 0.28 (low). This cyclical suppression profile is characteristic of scaffold-like mechanisms. Theater ratio (0.35): Low. The crisis machinery is functionally active — orders are issued, armies are conscripted, enemies are defeated. There is minimal performative content: the strongman acts, not merely gestures. This low theater distinguishes the crisis machinery from ritual institutions. However, the theater increases after the crisis (0.55) as the machinery's legitimacy rhetoric must justify why this time of exceptional power was necessary and how it was truly bounded.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. From the plebeian perspective, the crisis machinery is pure extraction (Snare): ordinary protections are removed, extraction is maximal, exit is impossible. From the state perspective, it is pure coordination (Rope): the machinery solves the collective action problem of unified response to existential threat. From the strongman perspective, it is mixed extraction (Snare): the strongman has supreme power but must release it, enforced by proscription or assassination. From the Senate perspective, it is mixed extraction (Tangled Rope): the Senate preserves itself but loses authority. From the Republican coalition perspective, it is temporary coordination (Scaffold): the machinery has a sunset and the coalition enforces it. From the civilizational analytical view, it appears as natural law (Mountain): every republic needs crisis machinery — but this is a false summit revealing that a contingent design is being naturalized. The perspectival gaps reveal the constraint's inner structure: the machinery distributes costs and benefits asymmetrically across time (immediate suppression, ultimate return), across actors (plebs lose, state gains, strongman is trapped), and across observational positions (powerless agents see extraction, institutional agents see coordination, analytical view sees necessity).
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from beneficiary/victim status and exit options. The state apparatus is a pure beneficiary with arbitrage exit (can choose to invoke or not invoke the machinery) → d ≈ 0.05 → f(d) ≈ -0.12 → χ ≈ ε × (-0.12) = negative (coordination, no extraction). The plebeian is a pure victim with trapped exit (cannot avoid the machinery) → d ≈ 0.95 → f(d) ≈ 1.42 → χ ≈ ε × 1.42 = very high extraction. The strongman is a beneficiary-but-trapped (has supreme power but must release it, enforced by threat) → d ≈ 0.65 → f(d) ≈ 1.0 → χ ≈ ε × 1.0 = high extraction despite beneficiary status. The Senate is mixed (benefits from state survival but loses authority) → d ≈ 0.55 → f(d) ≈ 0.75 → χ ≈ ε × 0.75 = moderate extraction. The Republican coalition is organized with mobile exit (can enforce or not enforce the sunset) → d ≈ 0.45 → f(d) ≈ 0.45 → χ ≈ ε × 0.45 = low-to-moderate extraction (Scaffold range). These derivations show why the same constraint is classified differently from each perspective: the d values differ, driving different χ and classification outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is legitimately a Tangled Rope from the overall system perspective. It has genuine coordination function (saving the state during existential threat — Rope component) AND genuine asymmetric extraction (suppressing checks and concentrating power — Snare component). The two functions are not contradictory; they are concurrent. The machinery coordinates the state's survival AND extracts from those without power to enforce the sunset. The mandatrophy would be irresolvable if the constraint were ONLY coordination or ONLY extraction — but it is manifestly both. The Tangled Rope classification says: this constraint solves a real coordination problem (temporal centralization during crisis) while simultaneously extracting from those who cannot resist (plebs, tribunes, assemblies). The measurement profile confirms this: extractiveness is high (0.68) but the theater is low (0.35), indicating that the functional coordination is real, not performative. If the machinery were pure extraction masked as coordination (Snare masquerading as Rope), the theater would be high. Here, theater is low: the machinery actually does coordinate state response to crisis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_existential_crisis,
    'What constitutes an existential threat sufficient to invoke the crisis machinery? Who determines the boundary?',
    'Historical analysis of declared vs. actual crises; comparison of invocation rates in genuine military threats (Hannibal, Gauls) vs. internal political conflicts (Gracchi, Catiline, Antony). Correlation between declared urgency and actual outcome.',
    'If the boundary is clear and narrowly enforced: crisis machinery is genuine temporary suspension (Scaffold/Rope). If the boundary is ambiguous and exploited: crisis machinery becomes standing extraction mechanism (Snare/Tangled Rope). If boundary definition itself becomes contested: reading is foreclosed by practice_drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_existential_crisis, empirical, 'Boundary definition for existential crisis — who determines, how narrow the gate').

omega_variable(
    sunset_enforcement_mechanism,
    'What institutional mechanism actually enforces the return of power after the crisis ends? Is it law, social norm, or threat of violence?',
    'Longitudinal tracking of term-limit compliance: how many strongmen returned power on schedule? Correlation between early return and institutional pressure vs. coerced return and resistance. Analysis of penalties for extending the dictatorship (proscriptions, civil war, assassination).',
    'If enforcement is automatic/legal: crisis machinery is self-limiting (Scaffold). If enforcement is social/norm-based: machinery is reliable but depends on elite consensus (Rope/Tangled Rope). If enforcement is by threat of violence: machinery is extraction trap masked as temporary (Snare). If enforcement fails: reading is foreclosed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_enforcement_mechanism, empirical, 'Enforcement mechanism for sunset clause — legal, social, or coercive').

omega_variable(
    crisis_machinery_versus_sibling_readings,
    'Does the existence of crisis machinery foreclose, coexist with, or influence the sibling readings of the Republican constitution?',
    'Textual analysis of sources: does the crisis machinery reading presuppose the Twelve Tables'' codification? Do the magistracies'' collegiality principles apply during dictatorship? Do the popular assemblies retain veto rights during senatus consultum ultimum? Does the Senate''s auctoritas override the dictator''s emergency powers or vice versa?',
    'If crisis machinery forecloses sibling readings: the constitution has multiple incompatible kernels (constitutional crisis). If sibling readings coexist: the constitution can be read coherently from multiple starting points. If crisis machinery influences siblings: the constitutional order has a hierarchy where emergency power is the supreme principle. Resolution determines reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_machinery_versus_sibling_readings, conceptual, 'Logical and structural relationships between crisis machinery reading and sibling readings').

omega_variable(
    extraction_inside_versus_outside_window,
    'Is extractiveness during the crisis window (0.68) structurally the same as extractiveness if the machinery persisted indefinitely?',
    'Counterfactual analysis: if a dictator abolished the term-limit and held power indefinitely, what would change? Would the beneficiary set expand? Would suppression remain at 0.78 or increase? Would theater_ratio increase as legitimacy rhetoric intensifies? Comparison with post-Republic regimes (Principate, Dominate) that made emergency powers permanent.',
    'If extractiveness would increase substantially under indefinite power: current classification as Tangled Rope (mixed coordination/extraction) is correct because the term-limit is doing real work. If extractiveness would remain stable: the crisis machinery is pure extraction masked as temporary (true Snare). If extractiveness would become infinite: the sunset is the entire constraint''s redemptive feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_inside_versus_outside_window, empirical, 'Comparative extractiveness: bounded emergency window vs. indefinite power').

omega_variable(
    false_summit_risk_on_mountain,
    'Is the civilizational analytical view''s classification of this machinery as a natural law (Mountain) a genuine insight into political necessity, or a naturalization of a contingent institutional design that benefits the state apparatus?',
    'Comparative constitutional analysis: do all stable republics include crisis machinery? Do republics without formal crisis provisions survive crises? Do republics with unchecked emergency powers remain republics? Cross-case evidence from Athens, Venice, United States, etc.',
    'If all functional republics require crisis machinery: Mountain classification is defensible (natural law of political order). If republics can survive crises without formal suspension mechanisms: Mountain classification is false summit (contingent design naturalized). If republics WITH formal machinery have worse outcomes: classification is inverted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_risk_on_mountain, conceptual, 'Whether crisis machinery is natural law or naturalized contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_republican_constitution__crisis_machinery, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rrc_crisis_theater_t0_precrisis, roman_republican_constitution__crisis_machinery, theater_ratio, 0, 0.5).
narrative_ontology:measurement(rrc_crisis_theater_t5_during, roman_republican_constitution__crisis_machinery, theater_ratio, 5, 0.35).
narrative_ontology:measurement(rrc_crisis_theater_t10_postcrisis, roman_republican_constitution__crisis_machinery, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(rrc_crisis_extractiveness_t0_precrisis, roman_republican_constitution__crisis_machinery, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rrc_crisis_extractiveness_t5_during, roman_republican_constitution__crisis_machinery, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(rrc_crisis_extractiveness_t10_postcrisis, roman_republican_constitution__crisis_machinery, base_extractiveness, 10, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rrc_crisis_suppression_t0_precrisis, roman_republican_constitution__crisis_machinery, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rrc_crisis_suppression_t5_during, roman_republican_constitution__crisis_machinery, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(rrc_crisis_suppression_t10_postcrisis, roman_republican_constitution__crisis_machinery, suppression_requirement, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_republican_constitution__crisis_machinery, enforcement_mechanism).
narrative_ontology:affects_constraint(roman_republican_constitution__crisis_machinery, roman_republican_constitution__legal_codification_twelve_tables).
narrative_ontology:affects_constraint(roman_republican_constitution__crisis_machinery, roman_republican_constitution__magistracies_and_collegiality).
narrative_ontology:affects_constraint(roman_republican_constitution__crisis_machinery, roman_republican_constitution__popular_assemblies_and_tribunate).
narrative_ontology:affects_constraint(roman_republican_constitution__crisis_machinery, roman_republican_constitution__senate_authority).

% DUAL FORMULATION NOTE:
% The crisis_machinery reading is part of the roman_republican_constitution constraint family. All sibling readings address different foundational principles of the same kernel — the Constitution itself. The crisis_machinery reading claims that the supremest principle is the constitution's ability to suspend itself. This influences (and partly forecloses) how the other readings can be interpreted: the magistracies are subject to dictatorship, the assemblies can be silenced, the Senate's auctoritas is subordinate to emergency power, and the written law yields to survival necessity. Each sibling reading should include the crisis_machinery in its network.affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
