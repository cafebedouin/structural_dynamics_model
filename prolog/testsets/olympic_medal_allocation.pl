% ============================================================================
% CONSTRAINT STORY: olympic_medal_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_olympic_medal_allocation, []).

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
 *   constraint_id: olympic_medal_allocation
 *   human_readable: Olympic Medal Allocation System
 *   domain: social/sports_governance
 *
 * SUMMARY:
 *   The Olympic medal allocation system presents itself as a meritocratic
 *   competition where nations and athletes earn medals through athletic
 *   excellence. In practice, medal distribution correlates far more strongly
 *   with nation-state wealth, government sports funding, infrastructure
 *   capacity, and sports science investment than with any measure of pure
 *   athletic talent. This creates a tangled rope: the system provides genuine
 *   coordination benefits (shared international rules, common performance
 *   metrics, global athletic standards) while simultaneously functioning as
 *   an extraction mechanism that concentrates medals among wealthy nations
 *   and redistributes prestige to those with resources to invest in athlete
 *   development. The constraint's theater ratio (0.68) reflects the
 *   performative invocation of merit to legitimize outcomes determined
 *   primarily by wealth. The system extracts athletic potential from
 *   developing nations, concentrating medals among wealthy states, while
 *   maintaining the institutional narrative that medals reflect pure
 *   performance.
 *
 * KEY AGENTS:
 *   - Wealthy Olympic Nations: Primary beneficiary (institutional/arbitrage) — capture majority of medals through infrastructure advantages; have exit options (can increase or decrease investment)
 *   - Developing Nation Athletes: Primary victim (powerless/trapped) — face structural barriers to competitive athlete development; cannot escape infrastructure deficit without emigrating
 *   - Developing Nations: Secondary victim (organized/constrained) — can strategically invest in high-ROI sports but cannot overcome wealth-based infrastructure gap; have constrained exit options
 *   - International Olympic Committee: Secondary beneficiary (institutional/arbitrage) — maintains legitimacy through merit narrative while system architecture redistributes medals to wealthy nations
 *   - Athletic Merit Principle: Theoretical victim (powerful/mobile) — invoked as justification but systematically violated by structural outcome patterns; persists as institutional theater
 *   - Global Athletics Community: Distributed victim (powerless/trapped) — collective epistemic good (fair international athletic standards) is corrupted by wealth-bias in medal allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(olympic_medal_allocation, 0.52).
domain_priors:suppression_score(olympic_medal_allocation, 0.65).
domain_priors:theater_ratio(olympic_medal_allocation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(olympic_medal_allocation, extractiveness, 0.52).
narrative_ontology:constraint_metric(olympic_medal_allocation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(olympic_medal_allocation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(olympic_medal_allocation, tangled_rope).
narrative_ontology:human_readable(olympic_medal_allocation, "Olympic Medal Allocation System").
narrative_ontology:topic_domain(olympic_medal_allocation, "social/sports_governance").

domain_priors:requires_active_enforcement(olympic_medal_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(olympic_medal_allocation, wealthy_nations).
narrative_ontology:constraint_beneficiary(olympic_medal_allocation, ioc_administrative_structure).
narrative_ontology:constraint_victim(olympic_medal_allocation, developing_nations).
narrative_ontology:constraint_victim(olympic_medal_allocation, athlete_merit_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING NATION ATHLETE (SNARE) — Trapped within a system where medal allocation is structurally biased by infrastructure and wealth. Despite meeting athletic performance standards, athletes from less resourced nations face systematic barriers: limited access to elite training facilities, coaching, nutritional support, and sports science. The constraint extracts their potential without offering exit — they cannot escape the nation-level infrastructure deficit without emigrating, which itself carries costs. Maximum experienced extraction.
constraint_indexing:constraint_classification(olympic_medal_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME NATION (TANGLED ROPE) — Benefits from the Olympic coordination mechanism (international athletic standards, shared training protocols, global competition framework) while also bearing extraction costs. Must invest heavily in athlete development infrastructure to compete, but has agency through strategic program selection (focus on specific sports where ROI is higher). Experiences both coordination (shared rules enable performance comparability) and extraction (medal systems reward existing infrastructure advantages). Exit is constrained but not blocked — can reduce Olympic investment.
constraint_indexing:constraint_classification(olympic_medal_allocation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: WEALTHY OLYMPIC NATION (ROPE) — Experiences the medal system as pure coordination. Existing sports infrastructure, government funding, and sports science capabilities align naturally with the performance metrics. The nation benefits from the coordination mechanism (standardized rules, international comparison framework) without experiencing extraction. Has arbitrage options: can increase/decrease Olympic investment based on cost-benefit. The constraint appears as voluntary participation in a beneficial coordination system.
constraint_indexing:constraint_classification(olympic_medal_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IOC (ROPE) — Benefits from the medal allocation system as a coordination and legitimation mechanism. The IOC uses medal tables to broadcast fairness and meritocracy while the system's structure perpetuates wealth-based advantage. The IOC has arbitrage options: can modify rules, adjust scoring, or reframe success metrics. Experiences minimal suppression — no actor can coerce the IOC; the IOC maintains authority through institutional consensus. Net beneficiary through coordination.
constraint_indexing:constraint_classification(olympic_medal_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ATHLETIC MERIT IDEAL (PITON) — The foundational claim of the Olympic system is that medals reward pure athletic excellence. This ideal persists as institutional theater despite structural evidence that wealth and infrastructure are dominant variables. The merit narrative is maintained through media storytelling, athlete hagiography, and celebration of exceptional individual performances that appear to contradict the structural bias. High theater ratio (0.68) reflects the performative invocation of merit to justify outcomes determined primarily by nation-state wealth allocation. The merit principle functions as inertial justification for a degraded coordination system.
constraint_indexing:constraint_classification(olympic_medal_allocation, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, resource allocation in any competitive system must reflect the aggregate economic capacity of participating agents. This constraint reflects a mathematical/economic law: systems optimizing for measurable performance naturally converge to outcomes predictable from input resource distribution. The Olympics cannot escape this law without fundamentally redesigning the constraint. However, the structural data contradicts the mountain classification: the Olympics IS a designed system, and alternative designs (resource caps, weighted performance metrics, regional quota systems) are technically possible. This perspective risks naturalizing a contingent institutional choice as an immutable law.
constraint_indexing:constraint_classification(olympic_medal_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(olympic_medal_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(olympic_medal_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(olympic_medal_allocation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(olympic_medal_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(olympic_medal_allocation, TR),
    TR >= 0.70.

:- end_tests(olympic_medal_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Olympic system extracts athletic potential and prestige from less wealthy nations and redistributes it to wealthy nations. The extraction is not absolute because middle-income nations can still compete effectively in specific sports and some athletes from developing nations achieve medals. However, the medal table's correlation with GDP is strong (r > 0.7 across Olympics), indicating systematic redistribution. The value reflects that extractiveness has increased over the interval (0.38 → 0.52) as global wealth inequality has grown and sports science advantage has compounded. Suppression (0.65): High. Barriers to developing-nation athlete development include limited access to elite coaching, sports medicine, training facilities, altitude training centers, and sports science support. Publication bias against developing-nation athletic success (media coverage concentrates on wealthy-nation athletes) suppresses visibility of counter-examples. Career risk for athletes in developing nations (uncertain return on investment in sports development) suppresses participation. However, suppression is not total (some developing nations field competitive athletes), and modern sports science diffusion is reducing information barriers. Theater ratio (0.68): High and increasing. The Olympic system's legitimacy narrative emphasizes meritocracy ('the fastest, highest, strongest') while its structural outcomes are determined primarily by nation-state wealth. Medal ceremonies, athlete storytelling, and media coverage maintain the merit narrative despite contradictory data. The theater has increased over the interval (0.52 → 0.68) as wealth-bias has become more apparent while the merit justification has intensified.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects fundamental disagreement on the system's nature. Wealthy nations and the IOC perceive primarily coordination benefits and natural fairness. Developing nations and their athletes perceive extraction masked by merit rhetoric. The piton perspective (athletic merit) reveals the mechanism: the merit narrative is maintained despite contradictory outcomes, functioning as institutional theater that justifies wealth-redistribution. The mountain perspective risks naturalizing a contingent institutional choice (Olympic competition structure) as an immutable law (wealth must correlate with performance). This gap is structural, not epistemic — it reflects genuine differences in how agents experience the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position within the extraction flow. Wealthy nations are beneficiaries with arbitrage options (exit is costless — they can reduce Olympic investment anytime) → d ≈ 0.10 → low/negative χ. Developing-nation athletes are victims with trapped exit → d ≈ 0.90 → high χ. Developing nations are organized victims with constrained exit (reduction of Olympic investment has political costs) → d ≈ 0.65 → moderate-high χ. The IOC is a beneficiary with arbitrage (controls the rules) → d ≈ 0.05 → low χ. The merit principle is a nominal beneficiary (cited in system legitimation) but practically a victim (systematically violated) → d ≈ 0.70 → high χ. These directionality values generate the perspectival gap: wealthy nations experience low χ (coordination), developing nations experience high χ (extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is resolved by recognizing that the system is NOT pure coordination (Rope) because it has asymmetric extraction, and it is NOT pure extraction (Snare) because it provides genuine coordination benefits through shared athletic standards and international competition framework. The tangled rope classification captures both functions: wealthy nations benefit from coordination without extraction; developing nations provide the coordination mechanism while bearing extraction costs. The theater ratio indicates that the system's legitimacy (merit narrative) increasingly diverges from its structural outcome (wealth redistribution), suggesting mandatrophy is degrading: as the wealth-bias becomes more apparent, the merit justification loses force. The system may scaffold toward alternative designs (resource-normalized metrics, regional quotas) or pitonize through institutional inertia (merit narrative persists despite evident falsity). Currently unresolved: no clear pathway to maintaining both coordination and fairness simultaneously within the current architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_infrastructure_separation,
    'Can athletic merit be meaningfully separated from the infrastructure and resource advantages that enable performance, or are they inseparably coupled?',
    'Longitudinal analysis of athlete performance trajectories comparing: athletes from wealthy nations with mediocre training vs athletes from developing nations with exceptional programs (Botswana distance running, Kenya athletics). Correlation analysis of medal rates with nation-state wealth, sports spending, and infrastructure capacity.',
    'If separable: merit-based allocation can be reformed through better measurement. If coupled: the merit principle cannot be salvaged — the system is necessarily wealth-redistributing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merit_infrastructure_separation, empirical, 'Separability of athletic merit from infrastructure advantage').

omega_variable(
    olympic_legitimacy_basis,
    'Does the Olympic system derive legitimacy from claimed meritocracy, from international coordination and peace-building, or from institutional tradition? Which is the actual base?',
    'Historical analysis of Olympic messaging; survey of nation-state participation rationale (diplomatic vs athletic); analysis of participation patterns when medals correlate poorly with expected outcomes. Examination of media framing shifts when wealth-bias becomes salient.',
    'If merit-based: the system loses legitimacy when wealth-bias is exposed. If coordination-based: the legitimacy persists regardless of medal distribution. If tradition-based: reform may be impossible without external shock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(olympic_legitimacy_basis, conceptual, 'Which legitimacy basis drives Olympic participation').

omega_variable(
    alternative_allocation_feasibility,
    'Are alternative medal allocation systems (resource-normalized scoring, regional quotas, sport-specific caps) technically implementable without destroying the coordination mechanism that makes the Olympics function?',
    'Game-theoretic analysis of alternative scoring systems; modeling of participation incentives under reformed rules; historical precedent analysis from sports that have modified allocation rules (e.g., tennis equal pay). Pilot testing in non-Olympic international competition.',
    'If feasible: reform is possible and the snare classification becomes scaffoldable. If infeasible: the system is architecturally locked into wealth-redistribution and can only degrade (piton) or dissolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_allocation_feasibility, empirical, 'Technical feasibility of alternative medal allocation mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(olympic_medal_allocation, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(olymp_tr_t0, olympic_medal_allocation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(olymp_tr_t32, olympic_medal_allocation, theater_ratio, 32, 0.63).
narrative_ontology:measurement(olymp_tr_t64, olympic_medal_allocation, theater_ratio, 64, 0.68).

% Extraction over time
narrative_ontology:measurement(olymp_be_t0, olympic_medal_allocation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(olymp_be_t32, olympic_medal_allocation, base_extractiveness, 32, 0.47).
narrative_ontology:measurement(olymp_be_t64, olympic_medal_allocation, base_extractiveness, 64, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(olympic_medal_allocation, global_infrastructure).
narrative_ontology:affects_constraint(olympic_medal_allocation, international_development_inequality).
narrative_ontology:affects_constraint(olympic_medal_allocation, sports_funding_concentration).

% DUAL FORMULATION NOTE:
% The Olympic medal allocation system can be decomposed into two structurally distinct constraints: (1) the merit principle (claimed ε ≈ 0.15, Mountain) — athletic excellence transcends wealth; (2) the wealth-correlation mechanism (actual ε ≈ 0.52, Tangled Rope) — medal distribution reflects nation-state resource capacity. These are linked by institutional contradiction: the IOC claims the first while structurally enforcing the second.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
