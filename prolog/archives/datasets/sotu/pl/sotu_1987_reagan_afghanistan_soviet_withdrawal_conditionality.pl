% ============================================================================
% CONSTRAINT STORY: sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, []).

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
 *   constraint_id: sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality
 *   human_readable: U.S. Conditionality on Soviet Withdrawal and Afghan Self-Determination (Reagan 1987)
 *   domain: foreign_policy/geopolitical_coercion
 *
 * SUMMARY:
 *   In 1987, President Reagan announced conditional U.S. support for the
 *   Afghan mujahideen resistance against Soviet occupation. The condition was
 *   explicit: U.S. would maintain aid only if a negotiated settlement
 *   included complete Soviet military withdrawal and genuine Afghan political
 *   self-determination—not a Soviet-friendly regime or puppet government.
 *   This constraint creates a structural tension between Cold War strategic
 *   interests (forcing Soviet retreat) and the stated principle (genuine
 *   Afghan autonomy). The mechanism operates through aid allocation decisions
 *   and diplomatic signaling: aid flows to mujahideen factions aligned with
 *   U.S. conditions, becomes conditional or is withheld for factions that
 *   reject the framework. The constraint exhibits coordination function
 *   (settling the war, establishing a negotiated outcome) alongside
 *   asymmetric extraction (constraining which political outcomes are
 *   acceptable to Afghan factions in exchange for military support). From
 *   different perspectives, the same constraint appears as a natural law of
 *   geopolitics (mountain), a temporary strategic tool (piton), coordination
 *   to defeat occupation (rope), mixed coordination-extraction (tangled
 *   rope), or pure coercion on dependent factions (snare). The theater ratio
 *   reflects that the stated commitment to 'Afghan self-determination' may be
 *   performative window dressing on unilateral Cold War strategy, or may
 *   represent a genuine constraint on acceptable outcomes—the evidence is
 *   ambiguous.
 *
 * KEY AGENTS:
 *   - United States (Reagan administration): Primary beneficiary (institutional/arbitrage) — uses conditionality to force Soviet withdrawal while maintaining influence over post-war Afghan state structure
 *   - Afghan Resistance Forces (non-Soviet-aligned): Secondary beneficiary/mixed victim (organized/constrained) — receive military aid and benefit from Soviet pressure, but constrained by U.S. conditions on acceptable political outcomes
 *   - Soviet Union: Primary victim (moderate/constrained) — faces extraction pressure (accept Soviet withdrawal) and suppression through military stalemate and economic cost
 *   - Soviet-Backed Afghan Factions: Secondary victim (powerless/trapped) — trapped between Soviet dependence and U.S. exclusion from political settlement
 *   - Cold War Bipolar System: Institutional actor (institutional/arbitrage) — maintains and reproduces the conditionality mechanism through great-power competition logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent Cold War strategy as inherent geopolitical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, 0.58).
domain_priors:suppression_score(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, 0.62).
domain_priors:theater_ratio(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, tangled_rope).
narrative_ontology:human_readable(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, "U.S. Conditionality on Soviet Withdrawal and Afghan Self-Determination (Reagan 1987)").
narrative_ontology:topic_domain(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, "foreign_policy/geopolitical_coercion").

domain_priors:requires_active_enforcement(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, afghan_resistance_forces).
narrative_ontology:constraint_beneficiary(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, us_cold_war_strategic_position).
narrative_ontology:constraint_victim(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, soviet_union).
narrative_ontology:constraint_victim(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, soviet_backed_afghan_factions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET-BACKED AFGHAN FACTIONS (SNARE) — Trapped in dependent relationship on Soviet backing; cannot exit conditionality without abandoning their sponsor and losing material support. U.S. conditionality narrows their political options to zero: continue war with Soviet backing (violates U.S. condition), or negotiate autonomy without Soviet support (abandons their patron). Maximum suppression and extraction — no alternatives exist.
constraint_indexing:constraint_classification(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SOVIET UNION (TANGLED ROPE) — Experiences genuine coordination incentive (negotiate withdrawal and transition to avoid protracted stalemate) alongside extraction pressure (accept U.S.-dictated political outcome or lose strategic foothold). Suppression is high (military quagmire, economic cost, ideological commitment) but not absolute — the Soviet Union retains agency to negotiate, withdraw, or escalate. The constraint exhibits both coordination function (settling the war) and asymmetric extraction (on Soviet terms dictated by U.S. leverage).
constraint_indexing:constraint_classification(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNITED STATES STRATEGIC POSITION (ROPE) — Benefits substantially from the conditionality mechanism. U.S. aid to mujahideen is presented as supporting self-determination (low-extraction framing) while actually constraining Soviet options and advancing U.S. Cold War objectives. Experiences constraint as coordination: coordinating with Afghan resistance to force Soviet withdrawal. Arbitrage exit option reflects ability to shift aid allocation, pivot diplomacy, or escalate/de-escalate without material cost.
constraint_indexing:constraint_classification(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AFGHAN RESISTANCE FORCES (TANGLED ROPE) — Experience genuine coordination function with U.S. interests (defeating Soviet occupation) alongside constraints on their political autonomy (must accept U.S. vision of 'Afghan self-determination'). Suppression moderate-high: continued fighting dependence on U.S. aid, and U.S. conditions set boundaries on acceptable political outcomes. But they retain agency to negotiate or resist U.S. preferences on post-war state structure. The constraint benefits them materially (weapons, funding) while extracting political autonomy.
constraint_indexing:constraint_classification(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: COLD WAR BIPOLAR STRUCTURE (PITON) — From a civilizational view, the conditionality is largely performative window dressing on unilateral power exercise. The stated commitment to 'Afghan self-determination' is theater: the constraint's real function is forcing Soviet strategic retreat. Once that function becomes impossible (Soviet Union dissolves) or redundant, the conditionality loses force. Theater ratio reflects the gap between the stated principle (support only if genuinely autonomous outcome) and the mechanism (support actual outcomes that advance U.S. interests, regardless of autonomy). The constraint persists through institutional inertia — Cold War reasoning — even as the Cold War ends.
constraint_indexing:constraint_classification(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From universal analytical perspective, the conditionality appears as an immutable structural feature of geopolitical leverage: any superpower supporting a proxy war will condition aid on alignment with its strategic objectives. This is presented as a natural law of international relations — conditionality is inherent to great-power competition. However, structural data contradicts the mountain classification, triggering false summit detection. The conditionality is a contingent institutional artifact, not a law of geopolitics.
constraint_indexing:constraint_classification(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, TR),
    TR >= 0.70.

:- end_tests(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The U.S. extracts significant benefits—forcing Soviet strategic retreat while maintaining influence over Afghan political outcomes—but the extraction is not maximal because genuine coordination incentives exist (both Soviet Union and Afghan resistance benefit from ending the war). The Soviet Union could escalate rather than negotiate, and Afghan factions could resist U.S. terms. The extractiveness value reflects sustained asymmetric pressure over time. Suppression (0.62): High. Barriers to Afghan faction non-compliance are substantial: Soviet-backed factions lose patronage if they resist U.S. terms; non-Soviet factions lose military aid if they pursue independent paths; diplomatic isolation and military stalemate enforce the constraints. However, suppression is not absolute—factions retain capacity to negotiate, escalate, or seek alternative patrons. Theater ratio (0.51): Moderate. The stated principle of 'Afghan self-determination' is partly performative (the outcome is largely predetermined by U.S. strategic interests) but also partially enforced (U.S. does condition aid, does exclude Soviet-backed outcomes). The theater increases over time as the constraint becomes institutionalized—initial strategic purpose (force Soviet withdrawal) may persist long after Soviet capacity to resist has ended, reflecting institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The U.S. institutional perspective (rope/arbitrage) sees the conditionality as coordination—aligning aid with strategic interests. The Soviet moderate perspective (tangled_rope/constrained) sees the conditionality as pressure to negotiate and retreat. The powerless Soviet-backed faction perspective (snare/trapped) sees the conditionality as exclusion and coercion—no acceptable path forward. The organized Afghan resistance perspective (tangled_rope/constrained) sees the conditionality as enabling but constraining—receives aid and military support in exchange for accepting U.S. political preferences. The civilizational analytical perspective risks seeing a mountain (natural law of geopolitics) but structural data triggers false summit detection—conditionality is a contingent Cold War tool, not an inherent feature of great-power competition. The perspectival gap reveals the constraint's hybrid nature: it coordinates on some dimensions (settling the war) while extracting asymmetrically on others (constraining Afghan autonomy).
 *
 * DIRECTIONALITY LOGIC:
 *   The United States derives positive directionality (d ≈ 0.15): beneficiary status, arbitrage exit option, and institutional power produce low effective extraction chi—the constraint is experienced by the U.S. as coordination and opportunity, not coercion. The Soviet Union derives moderate directionality (d ≈ 0.65): victim status, constrained exit (cannot escalate indefinitely without broader costs), and moderate power produce moderate-high chi—experienced as pressure but with negotiation options. Soviet-backed Afghan factions derive high directionality (d ≈ 0.92): victim status, trapped exit (abandon Soviet patron or lose Soviet aid), and powerless status produce very high chi—experienced as maximal extraction and coercion. Non-Soviet Afghan factions derive moderate-high directionality (d ≈ 0.58): mixed victim/beneficiary status (benefit from U.S. aid, constrained by U.S. conditions), constrained exit (cannot pursue independent political path without losing support), and organized power produce moderate chi—experienced as mixed extraction and opportunity. The perspectival gaps are significant: the U.S. sees coordination; the Soviets see pressure; Soviet-backed factions see exclusion; non-Soviet factions see conditional support.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival indexicality. From the U.S. beneficiary position, the constraint is rope (coordination). From the Soviet victim position, it is tangled_rope (coordination function exists—settling the war—but extraction pressure is severe). From the trapped Soviet-backed faction position, it is snare (pure coercion with no acceptable alternatives). From the analytical civilizational position, it appears as mountain (natural law) but is revealed as false summit—the constraint is contingent, not immutable. No single classification is 'correct' for the entire constraint structure; the correct analysis is the presheaf over all perspectives. The constraint simultaneously coordinates (settles the war, establishes a negotiated framework) and extracts (on whose terms, which outcomes are acceptable). This simultaneity is the diagnostic signature of tangled_rope at the central perspectives. The false summit risk arises from the mountain classification at the analytical level—great-power conditionality should not be naturalized as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_mechanism_enforcement,
    'Is the U.S. conditionality genuinely enforced (aid withheld when conditions violated), or is it performative signaling that aid flows regardless of compliance?',
    'Historical analysis of aid allocation decisions: Did U.S. reduce/suspend aid to Afghan factions that violated conditionality (e.g., rejected negotiations, pursued Soviet-aligned outcomes)? Document specific instances where conditions were tested and enforced or ignored.',
    'If genuinely enforced: conditionality is a real extraction mechanism constraining Afghan options. If performative: constraint is primarily theater, and Afghan factions had more agency than the snare classification suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_mechanism_enforcement, empirical, 'Whether U.S. actually withholds aid for non-compliance vs. flows regardless').

omega_variable(
    soviet_withdrawal_causality,
    'Did the U.S. conditionality causally drive Soviet withdrawal, or would the Soviets have withdrawn anyway due to internal costs and political change (Gorbachev''s new thinking)?',
    'Counterfactual analysis: comparison of Soviet withdrawal timeline with and without U.S. aid to mujahideen; assessment of Gorbachev''s reform agenda (glasnost/perestroika) and its independence from U.S. pressure; archival evidence of Soviet decision-making.',
    'If causally driven: U.S. conditionality was structurally powerful, extraction is high. If coincidental/overdetermined: U.S. took credit for structural change driven by Soviet internal factors, and extracted benefit through conditionality theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_withdrawal_causality, empirical, 'Whether U.S. conditionality causally drove Soviet withdrawal').

omega_variable(
    afghan_self_determination_definition_ambiguity,
    'What does ''Afghan self-determination'' mean operationally? Is it U.S.-defined (acceptable outcomes must exclude Soviets, must exclude communist factions) or genuinely Afghan-defined?',
    'Analysis of post-withdrawal Afghan state structure: Did it reflect Afghan preferences or U.S. preferences? Did Afghan factions (even those supported by U.S.) have agency to shape the outcome, or was the outcome predetermined by U.S. conditions?',
    'If U.S.-defined: conditionality is extractive (Afghan autonomy constrained by U.S. preferences). If genuinely Afghan: conditionality is primarily coordination (U.S. and Afghans jointly enforcing against Soviet-backed alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(afghan_self_determination_definition_ambiguity, conceptual, 'Whether self-determination was U.S.-defined or genuinely Afghan-defined').

omega_variable(
    false_summit_natural_law,
    'Is the conditionality presented as a natural law of geopolitics (''great powers must condition aid on alignment'') when it is actually a contingent choice by the Reagan administration?',
    'Historical comparison: did other Cold War administrations impose similar conditionality? Did the U.S. condition aid to other proxies with equivalent stringency? Or is the 1987 Afghanistan conditionality an anomaly reflecting Reagan''s specific anti-communist ideology?',
    'If natural law: constraint should remain classified as mountain. If contingent: constraint is a false summit, reclassified as tangled_rope or snare depending on observed extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether conditionality is natural law of geopolitics or contingent administration choice').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of Afghan political autonomy structural (enforced by external aid leverage) or partly internalized (Afghan factions internalize U.S. preferences as legitimate constraints)?',
    'Post-aid analysis: After U.S. aid flows (or ends), do Afghan factions continue to respect U.S. constraints on political outcomes, or do they pursue alternative paths? Does internalization persist after external enforcement ends?',
    'If primarily structural: suppression declines once U.S. leverage ends. If partly internalized: suppression persists through cognitive/institutional capture even after external constraints are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural leverage or internalized constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(afghan_cond_tr_t0, sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, theater_ratio, 0, 0.38).
narrative_ontology:measurement(afghan_cond_tr_t3, sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, theater_ratio, 3, 0.45).
narrative_ontology:measurement(afghan_cond_tr_t6, sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, theater_ratio, 6, 0.51).

% Extraction over time
narrative_ontology:measurement(afghan_cond_be_t0, sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(afghan_cond_be_t3, sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(afghan_cond_be_t6, sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, soviet_afghanistan_occupation).
narrative_ontology:affects_constraint(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, mujahideen_factional_cohesion).
narrative_ontology:affects_constraint(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, post_soviet_afghan_state_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is downstream of Soviet occupation (which it pressures) and upstream of post-war Afghan state structure (which it constrains). The conditionality mechanism is distinct from the occupation itself and represents a structural choice by the Reagan administration to leverage aid for political outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
