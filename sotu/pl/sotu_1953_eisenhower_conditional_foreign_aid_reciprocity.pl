% ============================================================================
% CONSTRAINT STORY: sotu_1953_eisenhower_conditional_foreign_aid_reciprocity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, []).

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
 *   constraint_id: sotu_1953_eisenhower_conditional_foreign_aid_reciprocity
 *   human_readable: Eisenhower's Conditional Foreign Aid Reciprocity (1953 SOTU)
 *   domain: foreign_policy/geopolitics/alliance_management
 *
 * SUMMARY:
 *   Eisenhower's 1953 State of the Union establishes a doctrine that U.S.
 *   military assistance to allied nations must be measured by the degree to
 *   which those nations 'earnestly strive' to defend their own independence
 *   and security. This constraint operationalizes conditionality: aid flows
 *   are contingent on observable behavioral compliance, creating incentive
 *   structures for allied burden-sharing. The mechanism creates structural
 *   extraction — recipient nations must demonstrate commitment through
 *   military expenditure, force posture, and political alignment with U.S.
 *   preferences — while maintaining the coordination narrative that aid is
 *   merely rewarding 'genuine' self-defense efforts. The constraint exhibits
 *   all six DR types across different observer positions, revealing how a
 *   single institutional mechanism can appear as coercive snare, genuine
 *   coordination, temporary scaffold, degraded ritual, or immutable natural
 *   law depending on the observer's structural position and temporal frame.
 *
 * KEY AGENTS:
 *   - United States (State Department & Treasury): Primary beneficiary (institutional/arbitrage) — controls aid allocation, gains compliance leverage, avoids aid to uncommitted allies, shapes allied military posture toward U.S. preferences
 *   - Allied Nations (Dependent/Vulnerable): Primary victims (powerless/trapped or moderate/constrained depending on capability) — face coercive conditionality, must demonstrate commitment at cost, lose sovereignty over defense budgets
 *   - NATO Alliance: Organized institutional actor (organized/constrained) — develops scaffold structure that eventually formalizes and de-coercifies bilateral conditionality into multilateral commitment
 *   - Developed Allied Nations (West Germany, France, UK post-1960s): Powerful actors with mobile exit (powerful/mobile) — initially constrained but gradually develop independent capacity, reducing extractiveness asymmetry
 *   - Cold War Threat (Soviet Union, Communist Expansion): Structural factor enabling enforcement — absence of alternative patrons makes U.S. conditionality binding
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent hegemonic arrangements as inescapable features of international politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, 0.52).
domain_priors:suppression_score(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, 0.58).
domain_priors:theater_ratio(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, tangled_rope).
narrative_ontology:human_readable(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, "Eisenhower's Conditional Foreign Aid Reciprocity (1953 SOTU)").
narrative_ontology:topic_domain(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, "foreign_policy/geopolitics/alliance_management").

domain_priors:requires_active_enforcement(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, united_states_treasury).
narrative_ontology:constraint_beneficiary(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, us_foreign_policy_control).
narrative_ontology:constraint_victim(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, allied_nations_defense_capacity).
narrative_ontology:constraint_victim(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, aid_recipient_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPELLED ALLIED NATION (SNARE) — A smaller ally dependent on U.S. military aid for survival faces coercive conditionality: aid is contingent on demonstrating 'earnest' self-defense, a criterion controlled by the U.S. and deployed retrospectively. The ally cannot exit: rejecting U.S. aid means military vulnerability; accepting aid means submitting to behavioral demands. Suppression is high: geopolitical isolation, strategic necessity, and absence of alternative great-power patrons. No coordination benefit visible from this perspective — only extraction of political compliance and burden-shifting.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED NATION DEFENSE ESTABLISHMENT (TANGLED ROPE) — Military and security planners within recipient nations experience genuine coordination with the U.S. (shared threat assessment, interoperability, burden-sharing against common adversary) alongside extraction (conditions on aid restrict domestic budget autonomy, force military posture to align with U.S. strategic preferences, create dependency). Exit is costly but possible: rearmament without U.S. aid is slow and expensive, but some nations execute it (France, West Germany post-1960s). The constraint coordinates defense cooperation while extracting compliance.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. STATE DEPARTMENT & TREASURY (ROPE) — From the American institutional perspective, conditional aid is pure coordination: it aligns allied military posture with U.S. strategic interests, ensures burden-sharing (allies contribute to their own defense, reducing U.S. outlays), and rewards compliant behavior. The constraint solves a free-rider problem — without conditions, allies would underinvest in defense while relying on U.S. protection. U.S. institutions experience the condition as an efficiency mechanism, not as extraction. They have arbitrage options: aid can be allocated to compliant allies and withheld from non-compliant ones.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATO COLLECTIVE DEFENSE FRAMEWORK (SCAFFOLD) — The NATO alliance evolves conditional aid into a formalized structure: Article 5 collective defense commits all members to reciprocal support, creating a scaffold for mutual burden-sharing. As NATO matures, the unilateral U.S. conditionality becomes less necessary — multilateral commitment replaces bilateral extraction. The scaffold has a sunset: as NATO members strengthen independently (West Germany, France), the constraint on aid allocation declines. By the 1970s-1990s, many recipient nations have sufficient capacity that U.S. aid is no longer coercive; it becomes reciprocal coordination.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: COMMITMENT EVALUATION RITUAL (PITON) — The mechanism for assessing 'earnest striving to defend' and 'genuine commitment' becomes increasingly theatrical over time. By the 1970s-1980s, the theater_ratio (0.64) reflects that evaluations are more about political messaging (demonstrating 'burden-sharing' to a U.S. domestic audience) than about actual measurement of ally capability or effort. Allies become skilled at performing commitment: military parades, visible military budgets, public statements of self-reliance. The ritual persists through institutional inertia even as the original functional purpose (ensuring realistic assessment of ally capacity) has degraded.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEVELOPED ALLIED NATION (TANGLED ROPE) — Wealthier allies (West Germany, France, UK) experience the constraint differently: they have genuine exit options (independent rearmament, nuclear deterrence, reduced dependence on U.S. aid). Yet they remain within the constraint for coordination benefits (NATO integration, technology access, intelligence sharing) and because the asymmetry has become mutual: the U.S. also needs their alliance. The constraint remains tangled (mixed coordination and extraction) but at much lower effective extraction — these actors have agency to negotiate terms.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears immutable: in anarchic international systems, security requires self-help; material aid alone cannot substitute for a nation's own commitment to defense; incentivizing burden-sharing is an inescapable feature of great-power alliance management. Conditionality seems like a law of geopolitics — no patron can subsidize a dependent indefinitely without demanding accountability. However, the structural data reveals this as a false summit: specific institutional arrangements (U.S. post-WWII dominance, bipolar Cold War structure, absence of alternative patrons) made unilateral conditionality possible. In other eras and structures, alliance aid flows without behavioral conditions.
constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, TR),
    TR >= 0.70.

:- end_tests(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant compliance from recipient nations — they must allocate defense budgets toward capabilities the U.S. prefers, participate in joint exercises, procure U.S. weapons systems, and align security doctrine with U.S. strategic preferences. However, extraction is not maximal (ε ≤ 0.66 for snare) because coordination benefits exist: shared defense against real threats, technology transfer, intelligence integration. The 0.52 value reflects that the constraint primarily benefits the U.S. (controlling aid flows, shaping alliance behavior) while imposing costs on recipients (defense autonomy constraints, budget rigidity, sovereignty compromise) but with some genuine mutual security gain. Suppression (0.58): High. Recipient nations have severely limited exit options: rejecting U.S. aid risks military vulnerability in a bipolar world; the absence of alternative patrons (Soviet aid is ideologically poisoned in the Western context) creates near-total dependency. Domestic political pressure within recipient nations to comply with U.S. conditions is intense — security elites depend on U.S. aid for legitimacy and capability. However, suppression is not absolute (≤ 0.60 for snare) because some nations do reduce reliance over time (France, West Germany), and the mechanism is not purely coercive (genuine security interests align). Theater ratio (0.64): Moderate-high. The evaluation of 'earnest commitment' becomes increasingly ritualized over the constraint's lifecycle. By the 1960s-1970s, aid decisions are driven more by Cold War political messaging (demonstrating U.S. commitment to burden-sharing at home) than by genuine assessment of ally capability or effort. Allies learn to perform commitment: military parades, visible budgets, public self-reliance rhetoric. The increase from 0.38 (early period, when assessment was more functional) to 0.64 (later period, when ritual dominates) reflects Goodhart drift — the measure of commitment becomes the target, replacing actual measurement of capability or effort.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range spans snare (trapped powerless allies), tangled rope (moderate-power allies with constrained exit), rope (U.S. institutional beneficiary), scaffold (NATO structural formalization), piton (ritualized evaluation), and false-summit mountain (naturalizing contingent arrangements). The key gaps are: (1) Beneficiary perspective sees coordination (rope) while victim perspective sees extraction (snare). (2) Early-period observers see functional conditionality (lower theater) while late-period observers see ritualization (higher theater). (3) Dependent allies see immutable constraint (trapped) while developed allies see negotiable constraint (mobile). (4) Civilizational analytical view risks naturalizing what is actually a contingent feature of Cold War bipolarity and U.S. hegemony. The constraint is not a single type — it is a presheaf over different observation contexts revealing how power asymmetries and temporal decay reshape the same institutional mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation maps power, exit options, and beneficiary/victim status to d values and f(d) extractiveness modulation. Powerless trapped allies (d → 0.95, f(d) ≈ 1.42) experience maximum effective extraction — they have no way out and bear full cost. Moderate constrained allies (d → 0.65, f(d) ≈ 1.00) experience moderate extraction — they have some agency and some benefit. Institutional beneficiaries with arbitrage options (d → 0.05, f(d) ≈ -0.12) experience negative effective extraction — they benefit and control allocation. Powerful mobile allies (d → 0.48, f(d) ≈ 0.60) experience reduced extraction — they have realistic exit options and can negotiate. The base_extractiveness (0.52) is scaled by these directionality-derived f(d) values and by spatial scope σ(S): global scope (σ=1.2) amplifies effective extraction. A powerless trapped ally at global scope experiences χ = 0.52 × 1.42 × 1.2 ≈ 0.89 effective extractiveness (snare territory). A powerful mobile ally at continental scope experiences χ = 0.52 × 0.60 × 1.0 ≈ 0.31 effective extractiveness (rope/scaffold territory). The divergence explains perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that 'burden-sharing coordination' and 'hegemonic extraction' are not mutually exclusive — they are the same institutional mechanism viewed from different structural positions. The U.S. genuinely needs allies to contribute to collective defense (coordination function). Allies genuinely need U.S. security guarantees and military capability (coordination function). Yet the mechanism of achieving this coordination is conditionality — which asymmetrically benefits the U.S. (controls allocation, shapes behavior) while imposing costs on recipients (defense autonomy, sovereignty constraints). The constraint is tangled rope because both functions (coordination and extraction) are structurally real. The analytical challenge is not choosing between 'it's coordination' and 'it's extraction' but modeling how a single institutional mechanism simultaneously solves a collective action problem (allies might free-ride on U.S. protection) while extracting compliance with U.S. preferences beyond what the security problem alone would require. The false summit detection fires because the civilizational analytical perspective attempts to naturalize this contingent arrangement as an inescapable feature of anarchic international politics — but in other eras (pre-WWII, post-Cold War) and structures (multipolar, symmetrical alliances), alliance aid flows without conditionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commitment_measurement_ambiguity,
    'What observable constitutes ''earnest commitment to self-defense''? Military spending as percentage of GDP? Participation in joint exercises? Weapons procurement choices? Combat deployments? Political rhetoric?',
    'Historical analysis of specific aid decisions (approval/denial) mapped to ally behavior metrics; correlation analysis between U.S. stated conditions and actual conditional funding',
    'If subjective/rhetorical: conditionality is largely a cover story for geopolitical preferences (Snare classification strengthens). If objective/measurable: conditionality reflects genuine coordination problem (Rope classification possible). Ambiguity enables extractive application while maintaining coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commitment_measurement_ambiguity, empirical, 'Operational definition of ''earnest commitment'' driving aid allocation').

omega_variable(
    extraction_vs_incentive_mechanism,
    'Does conditionality primarily extract compliance with U.S. preferences, or does it solve a genuine free-rider problem in alliance burden-sharing?',
    'Counterfactual analysis: would allies reduce defense spending without conditionality? Comparison with unconditional aid periods (e.g., Marshall Plan) to assess ally behavior sensitivity. Structural analysis of whether U.S. conditions reflect ally interests or diverge from them.',
    'If primarily extractive: constraint is Snare from recipient perspective, theater_ratio matters less. If primarily incentive-based: constraint is Rope/Tangled Rope, theater_ratio is lower. This determines whether the constraint''s core function is coordination or exploitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_incentive_mechanism, empirical, 'Whether conditionality solves alliance burden-sharing or extracts compliance').

omega_variable(
    power_asymmetry_temporal_decay,
    'Does the extractive power of U.S. conditionality decay as allied nations strengthen economically and militarily?',
    'Time-series analysis of extractiveness across Cold War periods: early phase (1948-1960, U.S. dominance, high extractiveness) vs mid-phase (1961-1975, allied development, declining extractiveness) vs late phase (1976-1991, multipolar, near-zero extractiveness for developed allies). Measurement of whether U.S. actually withholds aid or conditions become symbolic.',
    'If asymmetry decays: scaffold classification (sunset) is justified for developed allies; piton classification (theater) becomes dominant. If asymmetry persists: snare classification remains valid regardless of ally capability. Temporal structure of the constraint changes with power distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_asymmetry_temporal_decay, empirical, 'Rate of decay in extractive power as allied nations develop').

omega_variable(
    alternative_patron_availability,
    'In specific historical moments, could a recipient ally have sought material support from the Soviet Union or other non-U.S. sources as an exit option?',
    'Historical examination of Soviet aid offers, Chinese support alternatives, indigenous rearmament capacity for each major recipient. Assessment of exit costs: would an ally that switched patrons face greater burden (Soviet dependency, technology mismatch, ideological costs) or equivalent burden?',
    'If real alternatives existed: trapped exit is overstated (constrained or mobile more accurate). If no realistic alternatives: trapped exit is accurate. Exit_options assignments depend on ally-specific historical context, not universal Cold War structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_patron_availability, empirical, 'Existence and cost of switching to alternative patrons').

omega_variable(
    false_summit_natural_law_status,
    'Is the observed conditionality a natural law of alliance politics, or a contingent feature of post-WWII U.S. hegemony that would not appear in other structural configurations?',
    'Historical comparison: conditional aid in pre-WWII alliances, non-hegemonic alliance structures (e.g., Cold War Warsaw Pact alliances, contemporary multipolar regions). Structural analysis of whether conditionality requires hegemonic patron or emerges from symmetrical coordination.',
    'If natural law: mountain classification valid from analytical perspective. If contingent: false summit triggers; constraint reclassifies toward snare/tangled_rope from all perspectives. The naturalness of ''requiring burden-sharing proof'' is not self-evident — it reflects specific power asymmetries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Whether conditionality is a natural law or contingent institutional feature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sotu_tr_t5, sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, theater_ratio, 5, 0.52).
narrative_ontology:measurement(sotu_tr_t10, sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(sotu_be_t5, sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(sotu_be_t10, sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, resource_allocation).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, nato_collective_defense_article_five).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, marshall_plan_economic_reconstruction).
narrative_ontology:affects_constraint(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, cold_war_alliance_dependency_structure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1953_eisenhower_conditional_foreign_aid_reciprocity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
