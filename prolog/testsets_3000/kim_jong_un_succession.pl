% ============================================================================
% CONSTRAINT STORY: kim_jong_un_succession
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kim_jong_un_succession, []).

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
 *   constraint_id: kim_jong_un_succession
 *   human_readable: Kim Jong-un's Succession Plan and Dynastic Constraint
 *   domain: political/authoritarian_governance
 *
 * SUMMARY:
 *   Kim Jong-un's grooming of his daughter as a potential successor creates a
 *   structural constraint on North Korean politics by embedding dynastic
 *   continuity within a system designed to extract labor, ideology, and
 *   absolute obedience from the populace. The succession plan functions
 *   simultaneously as a coordination mechanism (ensuring regime stability and
 *   power transfer predictability) and as an extraction mechanism
 *   (perpetuating authoritarian rule and dynasty wealth concentration). The
 *   constraint exhibits high suppression (0.85) because exit from North
 *   Korean political control is nearly impossible for the populace; high
 *   extractiveness (0.68) because the system extracts resources, labor, and
 *   ideological compliance; and high theater (0.78) because the ideological
 *   apparatus (Juche, revolutionary mythology, cult of personality, now
 *   adapted for female succession) maintains legitimacy narratives detached
 *   from actual coercion mechanisms. The theater has increased over the
 *   measurement interval as propaganda effort has intensified to frame female
 *   succession as historically inevitable while maintaining patriarchal
 *   revolutionary mythology. The constraint mandatrophy is resolved by
 *   recognizing that the succession plan is structurally Tangled Rope from
 *   the analytical perspective (combining coordination and extraction) but
 *   appears as pure Snare from the powerless populace perspective (trapped
 *   with no exit) and as Rope from the dynasty perspective (pure coordination
 *   of power transfer).
 *
 * KEY AGENTS:
 *   - North Korean Populace: Primary victim (powerless/trapped) — subject to regime control with no exit mechanism; succession determines future oppression level
 *   - Military Hierarchy: Secondary victim and partial beneficiary (organized/constrained) — depends on regime continuation for positions and power but faces purge risk during succession instability
 *   - Party Cadre: Secondary victim and partial beneficiary (organized/constrained) — similar to military, vulnerable to succession-driven purges and reassignments
 *   - Kim Dynasty Core: Primary beneficiary (institutional/arbitrage) — succession plan preserves dynasty wealth, power, and legitimacy; experiences constraint as coordination problem solved
 *   - Regional Powers (China, Russia, USA): External observers (powerful/mobile) — can influence succession outcomes through diplomacy or pressure; experience constraint as temporary coordination challenge with management window
 *   - Ideological Legitimacy Apparatus: Institutional maintenance function (institutional/constrained) — propaganda, ideology education, cult of personality require constant narrative updating to frame female succession; theater-heavy because detached from actual power mechanics
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees constraint as irreducible hybrid combining genuine coordination function (power transfer, regime stability) with genuine extraction function (population control, dynasty perpetuation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kim_jong_un_succession, 0.68).
domain_priors:suppression_score(kim_jong_un_succession, 0.85).
domain_priors:theater_ratio(kim_jong_un_succession, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kim_jong_un_succession, extractiveness, 0.68).
narrative_ontology:constraint_metric(kim_jong_un_succession, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(kim_jong_un_succession, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kim_jong_un_succession, snare).
narrative_ontology:human_readable(kim_jong_un_succession, "Kim Jong-un's Succession Plan and Dynastic Constraint").
narrative_ontology:topic_domain(kim_jong_un_succession, "political/authoritarian_governance").

domain_priors:requires_active_enforcement(kim_jong_un_succession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kim_jong_un_succession, kim_family_dynasty).
narrative_ontology:constraint_victim(kim_jong_un_succession, north_korean_populace).
narrative_ontology:constraint_victim(kim_jong_un_succession, military_hierarchy).
narrative_ontology:constraint_victim(kim_jong_un_succession, party_cadre).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NORTH KOREAN POPULACE (SNARE) — Subject to succession constraint through no choice or exit mechanism. Political regime dictates all life circumstances: birth, education, employment, movement, ideology. Succession dynamics determine whether current oppressive structure persists or intensifies. No alternative leadership possible. Maximum suppression and extraction with zero exit options.
constraint_indexing:constraint_classification(kim_jong_un_succession, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY AND PARTY CADRE (TANGLED ROPE) — Organized actors with some internal coordination function (army structure, party hierarchy provide order and resource flows). But also subject to extraction: purges, reassignments, execution of rivals to succession control. Benefits from regime continuity (positions, power); constrained by vulnerability to purges during succession transitions. Succession uncertainty destabilizes their positions.
constraint_indexing:constraint_classification(kim_jong_un_succession, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL POWERS (SCAFFOLD) — External actors experience the succession as a temporary coordination problem with management window. Succession transitions are unstable periods offering potential influence opportunities or requiring stabilization efforts. Can exit through diplomatic pressure, sanctions, or military positioning. Sunset logic: succession will eventually stabilize into a new equilibrium, reducing uncertainty.
constraint_indexing:constraint_classification(kim_jong_un_succession, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: KIM DYNASTY CORE (ROPE) — Succession mechanism functions as coordination device preserving dynasty stability and perpetuating family rule. Female succession (if Ju-ae becomes heir) is portrayed as innovation maintaining legitimacy. Benefits from system that ensures power concentration and family wealth perpetuation. Experiences constraint as coordination problem solved by succession planning, not as extraction.
constraint_indexing:constraint_classification(kim_jong_un_succession, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IDEOLOGICAL LEGITIMACY APPARATUS (PITON) — Juche ideology, revolutionary legitimacy narratives, and cult-of-personality propaganda are ostensibly functional but increasingly performative. The succession constraint requires constant narrative maintenance: portraying dynasty continuation as inevitable historical progress, legitimizing female succession through reframed ideology. Theater ratio high because the ideological maintenance is detached from actual power mechanisms (coercion and military control). Institutional inertia maintains the apparatus despite reduced functional necessity.
constraint_indexing:constraint_classification(kim_jong_un_succession, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The succession constraint exhibits both coordination function (managing power transfer, preventing state collapse) and extraction function (perpetuating authoritarian dynasty, extracting labor and ideology from populace, suppressing leadership alternatives). Cannot be reduced to either pure coordination or pure extraction. Requires active enforcement (cult of personality, military loyalty mechanisms, party purges). Has both beneficiaries (dynasty) and victims (populace, rival elites). Analytical classification: Tangled Rope.
constraint_indexing:constraint_classification(kim_jong_un_succession, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kim_jong_un_succession_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kim_jong_un_succession, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kim_jong_un_succession, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kim_jong_un_succession, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kim_jong_un_succession, TR),
    TR >= 0.70.

:- end_tests(kim_jong_un_succession_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The succession constraint perpetuates a system that extracts labor, ideology, and life prospects from the North Korean populace with virtually no compensation or exit. The extraction is comprehensive: economic (labor extraction with minimal wage), political (absolute absence of choice or representation), social (control of movement, association, expression), and ideological (mandatory belief in regime narrative). The value reflects that while the succession plan itself is primarily a coordination mechanism for the dynasty, it serves the function of perpetuating an extractive system. Suppression (0.85): Very high. North Korea maintains possibly the highest suppression in modern states: border closure, execution of escape attempts, public executions of political prisoners, complete control of information, mandatory state ideology indoctrination, control of all economic activity. The succession plan requires maintaining and potentially intensifying this suppression (purges of succession rivals, elimination of alternative candidates). Theater ratio (0.78): High and rising. The ideological apparatus (Juche, revolutionary narrative, cult of personality) consumes significant resources and effort but functions primarily to maintain narrative legitimacy rather than to produce actual power. The transition to female succession required extensive ideological reframing (portraying Ju-ae as historically inevitable, adapting revolutionary mythology to female leadership) despite her having no obvious functional qualifications. This theater has increased over time as each generational transition requires new narrative construction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the dynasty and the populace is maximal. The Kim family core experiences succession as a coordination problem: how to transfer power smoothly, maintain legitimacy, prevent internal military coup, ensure continuity of dynasty wealth. They see the constraint as functional and beneficial — it solves the succession coordination problem. The North Korean populace experiences the same constraint as pure extraction: they have no voice in the succession, cannot exit the regime, and will be subject to whatever leadership emerges. The populace experiences maximum extraction and suppression because the succession mechanism perpetuates the very system that oppresses them. The military and party cadre occupy intermediate positions: they benefit from regime continuation (their positions depend on regime stability) but face purge risk during succession transitions (rivals must be eliminated, and new leadership may restructure power). The analytical observer recognizes that both perspectives are structurally correct: the succession mechanism simultaneously solves a genuine coordination problem (power transfer without state collapse) AND functions as an extraction mechanism (perpetuating authoritarian rule). This is the core mandatrophy: is the constraint primarily Rope (pure coordination) or primarily Snare (pure extraction)? The answer is Tangled Rope — it genuinely combines both functions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation produces different d values for different agents. The Kim dynasty core enters as beneficiaries with arbitrage exit options (they can negotiate succession terms, shape succession narrative, potentially negotiate safe exit if regime faces collapse risk) — derived d ≈ 0.05-0.15, producing negative or very low f(d), indicating they experience low or negative effective extraction (they extract from the system rather than the system extracting from them). The North Korean populace enters as victims with trapped exit (no ability to exit the regime, no alternative political options, escape attempts result in execution or family punishment) — derived d ≈ 0.95, producing high f(d) ≈ 1.42, indicating they experience maximum effective extraction. The military cadre enter as partial beneficiaries and partial victims: they benefit from regime continuation (institutional positions) but face victim status during succession (purge risk) and have constrained exit (can defect to China or South Korea but face high cost of family punishment, social execution, and loss of status) — derived d ≈ 0.50-0.65, producing moderate f(d), indicating mixed experience. The analytical observer is external, not embedded in the extraction flow, so derives d ≈ 0.72, producing f(d) ≈ 1.15, matching the analytical power atom canonical value.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The succession constraint demonstrates how the same structural arrangement can be Rope (pure coordination of power transfer) or Snare (pure extraction perpetuation) depending on perspective. The resolution recognizes that BOTH perspectives are structurally accurate: (1) From the dynasty's structural position, succession planning solves a genuine coordination problem — how to transfer power without regime collapse or civil war. This is a legitimate coordination function. (2) From the populace's structural position, the succession constraint perpetuates a system that extracts everything from them with no exit and no voice. This is genuine extraction. The constraint is neither pure coordination nor pure extraction — it is Tangled Rope because it SIMULTANEOUSLY serves both functions: it coordinates power transfer AND extracts from the populace. The mandatrophy is resolved by: (a) Recognizing that coordination and extraction are not mutually exclusive — a system can both solve a genuine problem AND exploit victims; (b) Declaring both beneficiaries (dynasty, military hierarchy) and victims (populace, cadre at risk during purges); (c) Requiring active enforcement (cult of personality, purges, propaganda, coercion); (d) Noting the extractiveness is high (0.68) but not extreme (not 0.95) because some portion of the constraint's function is legitimate coordination; (e) Noting the suppression is very high (0.85) and the theater is also high (0.78) because regime legitimacy requires both coercion AND narrative maintenance. The analytical classification (Tangled Rope) resolves the mandatrophy by rejecting the false binary (is it coordination or extraction?) in favor of the structural reality: it is a hybrid with both functions, and the framework must account for both simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    female_succession_legitimacy,
    'Can female succession maintain ideological legitimacy within the North Korean system''s patriarchal tradition and revolutionary mythology?',
    'Monitoring of official propaganda framing, elite acceptance signals, military loyalty demonstrations, and public compliance measures over 5-10 year succession period',
    'If legitimacy holds: succession constraint remains stable Snare/Tangled Rope. If legitimacy erodes: constraint shifts toward Scaffold (temporary) as system searches for alternatives or faces instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_succession_legitimacy, empirical, 'Whether female succession maintains ideological legitimacy in DPRK system').

omega_variable(
    military_factional_stability,
    'Will military factions accept subordination to a young female leader, or will succession trigger internal coups?',
    'Analysis of elite purges during transition, military appointment patterns, naval/ground force power dynamics, and WMD program control shifts',
    'If stable: constraint persists as Snare. If military factional instability emerges: constraint becomes Scaffold or degrades toward Piton as enforcement mechanisms fail.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_factional_stability, empirical, 'Military factional stability during female succession transition').

omega_variable(
    sanctions_regime_exit,
    'Can North Korea sustain its extraction of population labor and resources under intensified international sanctions during a succession transition?',
    'Monitoring of black market activity, smuggling routes, sanctions evasion effectiveness, economic output, and population survival indicators',
    'If extraction capacity maintained: Snare persists. If sanctions effectiveness increases: constraint shifts toward Scaffold (narrowing window) or Piton (degraded enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_regime_exit, empirical, 'Sanctions regime sustainability during succession period').

omega_variable(
    juche_ideology_substitutability,
    'Is Juche ideology functionally necessary for regime control, or is it purely performative theater maintainable regardless of actual power mechanics?',
    'Counterfactual analysis: comparison with other authoritarian regimes with lower ideological apparatus costs; examination of whether ideology changes are coordinated with coercion changes',
    'If ideology is necessary: theater ratio assessment is too high; constraint may be Tangled Rope rather than containing Piton perspective. If purely performative: Piton classification is confirmed; succession planning can substitute ideology without losing control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(juche_ideology_substitutability, conceptual, 'Functional necessity of Juche ideology versus performative theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kim_jong_un_succession, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kju_succ_tr_t0, kim_jong_un_succession, theater_ratio, 0, 0.65).
narrative_ontology:measurement(kju_succ_tr_t5, kim_jong_un_succession, theater_ratio, 5, 0.75).
narrative_ontology:measurement(kju_succ_tr_t10, kim_jong_un_succession, theater_ratio, 10, 0.78).

% Extraction over time
narrative_ontology:measurement(kju_succ_be_t0, kim_jong_un_succession, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(kju_succ_be_t5, kim_jong_un_succession, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(kju_succ_be_t10, kim_jong_un_succession, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kim_jong_un_succession, enforcement_mechanism).
narrative_ontology:affects_constraint(kim_jong_un_succession, north_korean_sanctions_regime).
narrative_ontology:affects_constraint(kim_jong_un_succession, northeast_asian_wmd_proliferation).
narrative_ontology:affects_constraint(kim_jong_un_succession, sino_north_korean_strategic_dependency).

% DUAL FORMULATION NOTE:
% The succession constraint is downstream of the underlying extraction system (DPRK regime control mechanisms) but represents a distinct structural problem: how to perpetuate dynasty while maintaining regime stability. The upstream constraint (regime control extraction) has higher extractiveness (near 0.95); the succession constraint has moderate-high extractiveness (0.68) because it functions partially as coordination. The succession constraint influences downstream geopolitical constraints through its impact on regime stability and WMD program control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kim_jong_un_succession, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
