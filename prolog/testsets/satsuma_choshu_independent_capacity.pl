% ============================================================================
% CONSTRAINT STORY: satsuma_choshu_independent_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_satsuma_choshu_independent_capacity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: satsuma_choshu_independent_capacity
 *   human_readable: Satsuma and Chōshū Independent Capacity Development (Late Tokugawa)
 *   domain: japanese_history/han_autonomy
 *
 * SUMMARY:
 *   The late Tokugawa bakufu maintained national order through a
 *   nested-container system: the shogun (outer container) extracted tribute
 *   and monopolized foreign contact from han (inner containers) in exchange
 *   for preventing regional warfare and maintaining currency/trade stability.
 *   Satsuma and Chōshū operated within this constraint as major daimyo but
 *   developed independent operational capacity — Satsuma through unofficial
 *   Western contact and steamship building; Chōshū through creating the
 *   Kiheitai militia units outside formal samurai structure. By the 1860s,
 *   both had military and logistical capacity equal to or exceeding bakufu
 *   forces in their regions. This capacity development did not immediately
 *   break the constraint; instead, it created asymmetry where inner
 *   containers could coordinate with each other (Satsuma-Chōshū alliance) and
 *   activate an alternative legitimacy source (the Imperial Court) that the
 *   bakufu's extraction system had previously rendered dormant. The
 *   constraint demonstrates how nested-container systems can become brittle
 *   when inner containers develop independent substrate: the 'mutual hostage'
 *   logic that stabilizes such systems (outer container holds inner-container
 *   hostages; inner containers threaten outer container's fiscal base) breaks
 *   down when inner containers can no longer be held or extracted from. The
 *   Restoration occurred not because capacity development made rebellion
 *   possible, but because capacity development plus legitimacy crisis made
 *   the bakufu's extraction mechanism unenforceable and the Imperial Court's
 *   coordination substrate more attractive.
 *
 * KEY AGENTS:
 *   - Tokugawa Bakufu: Outer container (institutional/arbitrage) — extracts tribute, maintains monopoly on foreign contact, coordinates national order; increasingly unable to enforce extraction as inner-container capacity grows
 *   - Satsuma Han: Inner container A (powerful/mobile) — develops military capacity through Western contact; benefits from domain autonomy but pays extraction costs; becomes primary beneficiary of Restoration as Satsuma oligarchy dominates Meiji
 *   - Chōshū Han: Inner container B (powerful/mobile) — develops Kiheitai militia and modern army units; parallel capacity trajectory to Satsuma; benefits from domain autonomy; becomes secondary beneficiary of Restoration
 *   - Other Han: Secondary inner containers (moderate/constrained) — lack capacity of Satsuma-Chōshū; trapped in extraction system; cannot coordinate independently
 *   - Imperial Court: Dormant legitimacy source (institutional/constrained) — subordinate to bakufu throughout Tokugawa but activates as alternative coordination frame during legitimacy crisis; becomes rallying point for anti-bakufu forces
 *   - Bakufu Fiscal Stability: Victim (powerless/trapped) — extraction system depends on tributary inflow and monopoly rents; becomes progressively unenforceable as inner-container capacity grows; eventually collapses when alternative legitimacy activates
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees the constraint as either immutable law of nested systems (false mountain) or contingent institutional arrangement dependent on legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(satsuma_choshu_independent_capacity, 0.52).
domain_priors:suppression_score(satsuma_choshu_independent_capacity, 0.65).
domain_priors:theater_ratio(satsuma_choshu_independent_capacity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(satsuma_choshu_independent_capacity, extractiveness, 0.52).
narrative_ontology:constraint_metric(satsuma_choshu_independent_capacity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(satsuma_choshu_independent_capacity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(satsuma_choshu_independent_capacity, tangled_rope).
narrative_ontology:human_readable(satsuma_choshu_independent_capacity, "Satsuma and Chōshū Independent Capacity Development (Late Tokugawa)").
narrative_ontology:topic_domain(satsuma_choshu_independent_capacity, "japanese_history/han_autonomy").

domain_priors:requires_active_enforcement(satsuma_choshu_independent_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(satsuma_choshu_independent_capacity, bakufu_central_authority).
narrative_ontology:constraint_beneficiary(satsuma_choshu_independent_capacity, han_regional_autonomy).
narrative_ontology:constraint_victim(satsuma_choshu_independent_capacity, bakufu_fiscal_stability).
narrative_ontology:constraint_victim(satsuma_choshu_independent_capacity, han_structural_subordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORMAL TOKUGAWA HIERARCHY (SNARE) — The structural subordination framework (sankin-kotai hostage system, tribute obligations, monopoly on foreign contact) becomes progressively less enforceable as Satsuma and Chōshū develop independent operational capacity. The hierarchy cannot exit this constraint without collapse; the outer container (bakufu) is trapped in a system where enforcement is becoming structurally impossible. Maximum extraction experienced by the institution that cannot adapt.
constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HAN INTERIOR / DOMAIN GOVERNANCE (ROPE) — From the perspective of Satsuma and Chōshū leadership, the constraint is pure coordination: managing domain resources, building military capacity, coordinating with other han to modernize Japanese capacity. The sankin-kotai and tribute system are overhead costs, but the han have genuine coordination problems (resource allocation, military development, diplomatic strategy) that the constraint nominally addresses. As capacity grows, this becomes coordination without effective subordination.
constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: BAKUFU EXTRACTIVE AUTHORITY (TANGLED ROPE) — The bakufu coordinates the national system (prevents regional wars, collects tribute, maintains currency, enforces trade routes) while simultaneously extracting massive resources through sankin-kotai residence requirements, harbor monopolies, and foreign trade control. The coordination function is genuine — the system does prevent han from warring; the extraction is equally genuine — Satsuma and Chōshū pay escalating costs for the privilege. As inner-container capacity grows, the extraction becomes less defensible because the coordination benefit diminishes: han can now provide their own security and diplomacy.
constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPERIAL LEGITIMACY FRAME (TANGLED ROPE) — The Tokugawa bakufu coordinates national governance while simultaneously extracting legitimacy from the Imperial Court (the Emperor is politically powerless but ritually essential). The bakufu coordinates military order and prevents civil conflict; simultaneously it extracts the Emperor's authority and limits the court's resources. This is not discovered as a Snare until 1860s — the coordination was functional and the extraction was accepted as natural. Legitimacy crisis (Perry, unequal treaties) activates the dormant alternative: the Imperial Court becomes the coordination substrate for anti-bakufu forces.
constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEIJI REFORMERS / FORWARD VIEW (SCAFFOLD) — From the perspective of those who orchestrated the Restoration, the entire constraint is temporary: a coordination mechanism (Tokugawa order) with an embedded sunset (legitimacy crisis and external pressure making central coordination impossible). The Satsuma-Chōshū alliance plus Imperial backing constitute a coalition that sees the bakufu system as a failed coordination attempt whose alternatives are now superior. The Meiji structure itself replicates the same tangled rope (central government coordination + extraction from prefectures), but with a sunset: prefectural abolition in 1871 resolves the tension.
constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRIBUTARY SYSTEM APPARATUS (PITON) — The formal structure of bakufu-han relations (sankin-kotai, the daimyo council, formal hierarchy) becomes increasingly performative as real power (military capacity, foreign contacts, resource logistics) migrates to Satsuma and Chōshū. By the 1860s, the formal apparatus persists through institutional inertia — meetings still occur, tribute is still collected, formal rank is still respected — but actual authority flows through alternative channels (the Boshin Council, the Imperial Court, military capacity). The theater ratio is high because enormous resources are devoted to maintaining the formal fiction of bakufu supremacy.
constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NESTED AUTONOMY (MOUNTAIN) — From a civilizational perspective, the constraint appears to be an immutable structural feature of nested-container systems: inner containers will always develop capacity that challenges outer-container authority. This is framed as a law of institutional dynamics — the 'mutual hostage' logic that makes such systems stable but brittle. However, the structural data contradicts this mountain classification. The bakufu's ability to suppress inner-container capacity development was contingent on legitimacy and information control, both of which failed. This is a false summit: naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(satsuma_choshu_independent_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(satsuma_choshu_independent_capacity, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(satsuma_choshu_independent_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(satsuma_choshu_independent_capacity, TR),
    TR >= 0.70.

:- end_tests(satsuma_choshu_independent_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The bakufu's extraction from Satsuma and Chōshū is substantial — sankin-kotai residence requirements, tributary obligations, harbor monopolies, foreign trade control monopoly — but begins at lower levels in the early Tokugawa (0.35) and escalates as han capacity visibly grows, producing efforts at suppression (higher extraction to limit han autonomy). The extractiveness measures the flow of resources and authority to the bakufu relative to the coordination benefit provided. Early in the interval, extraction is moderate because coordination function is real (prevents han warfare). By 1860, extraction dominates because coordination is redundant — Satsuma and Chōshū can coordinate among themselves without bakufu mediation. The endpoint value (0.68 in measurements reflects structural capacity gap, not measured extraction) shows how the constraint transitions from Rope (early coordination) through Tangled Rope (mixed coordination and extraction) to approaching Snare (extraction without coordination). Suppression (0.65): High. Multiple mechanisms: (1) sankin-kotai hostage system creates permanent vulnerability; (2) Edo spies monitor domain activity; (3) restrictions on foreign contact prevent independent diplomacy; (4) formal hierarchy requires deference in all official contexts; (5) military monopolies prevent formal armament; (6) publication controls prevent information circulation. Suppression is high throughout but becomes less effective over time as Satsuma-Chōshū develop covert capacity development channels and informal alliances. Theater ratio (0.58): Moderate-high. The formal apparatus of bakufu authority (council meetings, tribute ceremonies, rank hierarchy) persists and requires significant resource investment, but actual power increasingly flows through informal channels (Satsuma-Chōshū alliance, Western contact networks, Imperial Court liaison). By 1860, the formal structure is substantially performative — ceremonies occur, tributes are paid, official hierarchy is respected — but decisions flow through military capacity and coalition logistics, not through formal channels.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical classification reveals structural transitions that mono-perspective analysis misses. The bakufu believes it maintains Rope throughout the period — the legitimacy of outer-container authority is not questioned until the 1860s legitimacy crisis. Satsuma and Chōshū experience the constraint as evolving: Rope (early — accept domain autonomy tradeoff) → Tangled Rope (mid — extraction becomes burden relative to coordination benefit) → Snare (late — extraction without coordination, exit becomes desirable). The analytical observer risks seeing Mountain (immutable nested-container dynamics) but the structural data reveals Tangled Rope — the bakufu's authority is contingent on legitimacy and information control, both of which failed. The Imperial Court perspective is the diagnostic linchpin: it appears throughout the period as a separate actor (subordinate but present) but only becomes structurally relevant when legitimacy crisis validates the dormant alternative. The Meiji reformers see Scaffold — the entire bakufu system is a failed coordination mechanism with sunset logic, and the alternative (centralized but legitimated through Imperial authority) is superior for the new international context.
 *
 * DIRECTIONALITY LOGIC:
 *   Deriving d values: The bakufu occupies the institutional/arbitrage position — it holds most power and has multiple exit options (maintain supremacy, negotiate constitutional role, flee to Kyoto, align with one han against others). As inner-container capacity grows and the bakufu's options narrow (all paths lead to reduced authority), the exit options degrade from arbitrage to constrained. The d value rises from ~0.10 (beneficiary with abundant options) to ~0.50 (symmetric position — equal constraints on both extractors and extracted). Satsuma-Chōshū occupy the powerful/mobile position early (can develop capacity, still constrained by suppression) with d ≈ 0.45-0.50. As capacity becomes undeniable and suppression becomes infeasible, exit options upgrade from constrained to mobile, and d rises to 0.70-0.75. Other han remain at moderate/constrained with d ≈ 0.75-0.80 throughout — they cannot match Satsuma-Chōshū capacity and are trapped in the extraction system. The Imperial Court occupies institutional/constrained — it has symbolic authority but no military power and bakufu control over information. d ≈ 0.35. When legitimacy crisis activates the dormant alternative, the Imperial Court's position becomes institutional/mobile (still no military power, but now directly relevant to coordination), and d remains ~0.35 but the experienced chi changes radically because the constraint type (from Rope/Tangled Rope to Tangled Rope/Scaffold) changes. No directionality overrides are needed — the structural derivation captures the dynamics correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT LIFECYCLE EXEMPLAR: This constraint demonstrates mandatrophy resolution through structural decomposition. Early period (Rope) and late period (Snare/Tangled Rope) are not competing classifications of the same constraint — they are the same constraint exhibiting different properties as inner-container capacity developed. The mandatrophy (conflicting classifications from different perspectives) is resolved by recognizing that the constraint exhibits: (1) genuine Rope function early (coordination prevents han warfare), (2) genuine Tangled Rope structure mid-period (coordination + extraction coexist), (3) disintegrating Tangled Rope late-period (extraction exceeds coordination benefit). The measurements show extractiveness rising from 0.35 to 0.75, which explains why early perspectives classify as Rope (low ε supports Rope gate) and late perspectives classify as Tangled Rope or Snare (high ε, high suppression). The theater ratio rising from 0.35 to 0.62 indicates increasing performativity — the formal structure persists but its functional role degrades. The Meiji Restoration 'resolves' the mandatrophy by terminating the constraint (bakufu dissolved, han abolished, new centralized government installed). But the Meiji government itself replicates the same Tangled Rope structure with new actors (central government vs. prefectures / military factions) — suggesting the constraint structure is not 'solved' but rather reset at a new scale. The forward-looking scaffold perspective acknowledges this: the Meiji system is itself a temporary coordination mechanism whose extraction will eventually activate inner-container (prefectural/military) capacity development. The mandatrophy is not resolved by choosing the 'correct' type but by understanding that the constraint's type depends on the developmental stage of inner-container capacity relative to outer-container enforcement capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_development_timing,
    'Did Satsuma and Chōshū develop independent capacity intentionally (as planning for Restoration) or opportunistically (modernizing domain governance within Tokugawa constraints)?',
    'Historical documentation of decision-making: internal han records, correspondence with Western contacts, military planning timelines. Evidence of explicit anti-bakufu intent vs. domain modernization intent.',
    'If intentional planning: the alliance was strategic from ~1850; extraction mechanism was always known and deliberately built around. If opportunistic: the constraint''s extractive power emerges gradually; the Restoration catalyzes pre-existing capacity rather than requiring it. Changes timing of when the constraint transitions from Rope to Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_development_timing, empirical, 'Whether independent capacity development was intentional anti-bakufu strategy or domain modernization').

omega_variable(
    external_pressure_necessity,
    'Could the Meiji Restoration have occurred without external pressure (Perry, unequal treaties, Western military threat) as the legitimacy crisis trigger?',
    'Counterfactual analysis from historical sources: what were the stated grievances and mobilization frames? How much was external threat vs. internal structural tension? Comparison with other han that developed capacity but didn''t participate in Restoration.',
    'If external pressure was necessary: the constraint''s activation depended on exogenous shock; the nested-autonomy structure would have remained stable indefinitely without legitimacy crisis. If internal structure was sufficient: the Restoration was inevitable once capacity developed; external pressure accelerated inevitable structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_pressure_necessity, empirical, 'Whether external pressure was necessary for legitimacy crisis and Restoration activation').

omega_variable(
    bakufu_awareness_level,
    'To what degree was the bakufu aware of Satsuma and Chōshū capacity development? Did suppression efforts fail because capacity growth was hidden, or because suppression was infeasible?',
    'Bakufu official records, spy reports, diplomatic correspondence. Documentation of: (a) what the bakufu knew and when, (b) what suppression attempts were made, (c) why suppression failed or was not attempted.',
    'If hidden: the constraint represents information asymmetry; the bakufu''s entrapment was partly epistemic. If visible but unsuppressible: the constraint represents power asymmetry; the bakufu saw capacity growth but lacked enforcement mechanisms. Affects classification of bakufu perspective — information asymmetry vs. power constraint are different extraction mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bakufu_awareness_level, empirical, 'What the bakufu knew about Satsuma and Chōshū capacity development and why suppression failed').

omega_variable(
    alternative_coalition_viability,
    'Could an alternative coalition (not Satsuma-Chōshū-Imperial) have achieved Restoration? Were these domains structurally necessary?',
    'Historical analysis of other han capacity levels, military resources, diplomatic networks. Examination of non-Satsuma-Chōshū alternatives that were considered or attempted. Assessment of what made Satsuma-Chōshū uniquely capable vs. strategically unique.',
    'If other coalitions were viable: the constraint''s resolution was contingent on specific actors; the mechanism is coalition formation rather than autonomous capacity. If Satsuma-Chōshū were necessary: the constraint''s resolution was structurally determined; capacity development led inevitably to these actors dominating Restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coalition_viability, empirical, 'Whether Satsuma-Chōshū were structurally necessary for Restoration or just one viable coalition').

omega_variable(
    meiji_replication_logic,
    'Does the Meiji government replicate the same Tangled Rope structure (central coordination + extraction from prefectures/regions) or achieve genuine federation?',
    'Analysis of Meiji fiscal structure, prefectural autonomy limits, resource extraction mechanisms. Comparison of Meiji extraction to Tokugawa extraction; assessment of whether internal capacity development (regional military, industrial) was suppressed as bakufu suppressed han capacity.',
    'If Meiji replicates: the constraint structure is cyclical; nested-container dynamics persist at new scale and will eventually activate again. If Meiji achieves genuine federation: the constraint is resolved through institutional redesign (devolved sovereignty rather than hierarchical extraction). Changes assessment of whether Restoration ''solved'' the problem or postponed it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_replication_logic, empirical, 'Whether Meiji government replicates or overcomes the Tangled Rope structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(satsuma_choshu_independent_capacity, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(satcho_theater_1780, satsuma_choshu_independent_capacity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(satcho_theater_1800, satsuma_choshu_independent_capacity, theater_ratio, 20, 0.4).
narrative_ontology:measurement(satcho_theater_1820, satsuma_choshu_independent_capacity, theater_ratio, 40, 0.45).
narrative_ontology:measurement(satcho_theater_1840, satsuma_choshu_independent_capacity, theater_ratio, 60, 0.55).
narrative_ontology:measurement(satcho_theater_1860, satsuma_choshu_independent_capacity, theater_ratio, 80, 0.62).

% Extraction over time
narrative_ontology:measurement(satcho_extract_1780, satsuma_choshu_independent_capacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(satcho_extract_1800, satsuma_choshu_independent_capacity, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(satcho_extract_1820, satsuma_choshu_independent_capacity, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(satcho_extract_1840, satsuma_choshu_independent_capacity, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(satcho_extract_1860, satsuma_choshu_independent_capacity, base_extractiveness, 80, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(satsuma_choshu_independent_capacity, enforcement_mechanism).
narrative_ontology:affects_constraint(satsuma_choshu_independent_capacity, bakufu_tribute_monopoly).
narrative_ontology:affects_constraint(satsuma_choshu_independent_capacity, sankin_kotai_hostage_system).
narrative_ontology:affects_constraint(satsuma_choshu_independent_capacity, imperial_court_dormancy).
narrative_ontology:affects_constraint(satsuma_choshu_independent_capacity, han_military_suppression).

% DUAL FORMULATION NOTE:
% This constraint is part of the Tokugawa nested-autonomy family. Upstream: sankin-kotai hostage system and bakufu tribute monopoly are the enforcement substrates that make this constraint possible. Downstream: the constraint family (han capacity development, Imperial Court activation, Restoration coalition formation) leads directly to the constraint's termination. The bakufu_tribute_monopoly story has higher ε (pure extraction mechanism); this story captures the hybrid coordination-extraction structure. The han_military_suppression story captures the suppression mechanisms that the bakufu deployed to limit capacity development. All four constraints resolved simultaneously when the Restoration dismantled the outer container.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
