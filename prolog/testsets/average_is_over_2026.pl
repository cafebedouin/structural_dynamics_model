% ============================================================================
% CONSTRAINT STORY: average_is_over_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_average_is_over_2026, []).

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
 *   constraint_id: average_is_over_2026
 *   human_readable: The AI-Talent Barbell Economy
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The AI-talent barbell economy of 2026 represents a structural separation
 *   of the labor market into two tiers with minimal mobility between them.
 *   The top tier — composed of elite cognitive talent with deep AI literacy
 *   and access to cutting-edge models — experiences exponential returns from
 *   AI-augmentation: their productivity multipliers, creativity leverage, and
 *   decision-making speed are amplified by orders of magnitude. The bottom
 *   tier — median-skill workers whose competencies were traditionally built
 *   through incremental upskilling — finds that traditional pathways no
 *   longer lead to stable middle-class outcomes. AI has not eliminated jobs
 *   uniformly; rather, it has bifurcated the skill distribution: roles that
 *   require intuitive judgment, pattern recognition at scale, and creative
 *   recombination of ideas now require elite-level talent to access, while
 *   routine cognitive labor has been eliminated or commodified into low-wage,
 *   high-turnover positions. The constraint is maintained by three
 *   reinforcing mechanisms: (1) training concentration — elite AI training is
 *   expensive and gatekept by capital owners and top universities, (2)
 *   income-based access — elite talent can afford premium tools and compute,
 *   creating a positive feedback loop, and (3) cultural legitimacy — the
 *   meritocracy narrative persists despite its structural invalidity,
 *   suppressing demand for redistributive intervention. This constraint
 *   exhibits all six classification types from different perspectives,
 *   revealing the perspectival depth of how inequality becomes
 *   institutionalized.
 *
 * KEY AGENTS:
 *   - Elite AI-Native Talent: Primary beneficiary (institutional/arbitrage) — experiences exponential returns, abundant exit options, coordination benefit from AI leverage
 *   - Median Skill Workers: Primary victim (powerless/trapped) — no viable exit from barbell structure, effort-based upskilling no longer functional, wage compression
 *   - Displaced Cognitive Labor: Secondary victim (moderate/constrained) — eliminated or restructured roles, retraining barriers, age and geographic constraints on mobility
 *   - Capital Owners / AI Infrastructure: Primary beneficiary (institutional/arbitrage) — gatekeep access to compute and models, extract licensing rents, control talent allocation mechanisms
 *   - High-Touch Service & Skilled Trades: Secondary actor (moderate/mobile) — partially insulated from AI substitution, but entry barriers rising, supply constraints creating mixed benefits and extraction
 *   - Redistribution Coalition: Organized agent (organized/constrained) — proposes sunset mechanisms (UBI, job guarantees) to bridge transition, but political viability contested
 *   - Meritocracy Narrative Maintainers: Institutional (institutional/arbitrage) — educational institutions, media, policymakers sustain legitimacy narrative despite empirical collapse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(average_is_over_2026, 0.58).
domain_priors:suppression_score(average_is_over_2026, 0.62).
domain_priors:theater_ratio(average_is_over_2026, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(average_is_over_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(average_is_over_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(average_is_over_2026, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(average_is_over_2026, snare).
narrative_ontology:human_readable(average_is_over_2026, "The AI-Talent Barbell Economy").
narrative_ontology:topic_domain(average_is_over_2026, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(average_is_over_2026, elite_ai_native_talent).
narrative_ontology:constraint_beneficiary(average_is_over_2026, capital_owners_of_ai_infrastructure).
narrative_ontology:constraint_victim(average_is_over_2026, median_skill_workers).
narrative_ontology:constraint_victim(average_is_over_2026, displaced_cognitive_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIAN SKILL WORKER (SNARE) — Trapped in a labor market where AI has rendered traditional skill-building pathways obsolete. Cannot retrain into elite tier without exceptional intrinsic talent. No viable exit from the barbell structure — constrained to low-automation-resistant roles with compressed wages. Maximum experienced extraction: career trajectory determined by initial talent distribution, not effort.
constraint_indexing:constraint_classification(average_is_over_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED COGNITIVE LABOR (SNARE) — Former routine cognitive jobs (bookkeeping, junior analysis, content moderation, basic programming) have been eliminated or restructured as low-wage, high-turnover positions. Retraining programs exist but do not bridge the gap to elite-tier compensation. Exit options are constrained: geographic mobility limited, sector transitions costly, age discrimination compounds. Extraction rate is severe: income floors have collapsed and bargaining power is minimal.
constraint_indexing:constraint_classification(average_is_over_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE AI-NATIVE TALENT (ROPE) — Experiences the constraint as pure coordination: AI systems amplify cognitive leverage, enabling elite talent to extract disproportionate value from each hour worked. Arbitrage options abundant: startups, hedge funds, tech giants compete for top talent with equity and compensation packages. The constraint is a beneficial coordination structure for this agent — access to AI leverage is what they organize themselves to capture.
constraint_indexing:constraint_classification(average_is_over_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPITAL OWNERS / AI INFRASTRUCTURE (ROPE) — Own the hardware, models, and API endpoints that gate access to AI tools. Experience the barbell economy as a coordination success: elite talent + AI infrastructure complementarity creates exponential value capture. Arbitrage options abundant: monopolistic pricing on compute, licensing fees, equity positions in AI-native companies. The constraint is self-reinforcing coordination from this agent's perspective.
constraint_indexing:constraint_classification(average_is_over_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-TOUCH SERVICE & SKILLED TRADES (TANGLED ROPE) — Experiences mixed signals: some roles (nursing, electrician, HVAC, plumbing) remain AI-resistant and see wage growth from labor scarcity. But entry barriers have risen (licensing, apprenticeship costs, credential inflation) and the perception of AI disruption has created educational bifurcation — fewer young people invest in trade training, creating supply constraints. Mobile exit options exist but require geographic flexibility and physical relocation. Some extraction through credential gatekeeping and apprenticeship bottlenecks, but also genuine coordination benefit from supply scarcity.
constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: UBI / REDISTRIBUTION COALITION (SCAFFOLD) — Organized movements (labor unions, policy advocates, some institutional investors) propose redistributive mechanisms (UBI, wealth taxes, job guarantees) as temporary corrections to the barbell structure. These are intentionally sunset mechanisms: designed to bridge the labor-market transition until new equilibrium emerges (or until the constraint structure degrades enough that redistribution becomes politically unsustainable). Theater ratio is moderate because the proposals address real structural data but implementation remains contested and politically volatile.
constraint_indexing:constraint_classification(average_is_over_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: MERITOCRACY NARRATIVE (PITON) — The institutional story that 'talent and hard work determine outcomes' persists as a cultural narrative despite collapsing validity. Educational institutions, media, and policymakers continue to frame economic outcomes as merit-based even as the barbell structure renders effort and incremental skill-building functionally irrelevant for the median cohort. This narrative is performative: it legitimates existing inequality through a narrative frame that has lost structural function. Theater ratio high because maintenance of the meritocracy myth requires constant rhetorical work with diminishing empirical support.
constraint_indexing:constraint_classification(average_is_over_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a long-term lens, the barbell economy appears as an inevitable consequence of technological leverage: any technology that amplifies cognitive output will create multiplicative returns for high-skilled agents, producing inherent bimodal distributions in economic outcomes. This perspective risks naturalizing what is actually a contingent institutional arrangement (access to AI, training concentration, capital allocation mechanisms) as an immutable law of technological change.
constraint_indexing:constraint_classification(average_is_over_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(average_is_over_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(average_is_over_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(average_is_over_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(average_is_over_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(average_is_over_2026, TR),
    TR >= 0.70.

:- end_tests(average_is_over_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint extracts significantly from the median and displaced cohorts through income compression, reduced bargaining power, and elimination of traditional upskilling pathways. The extraction rate reflects sustained suppression of exit options and concentration of AI-access benefits at the top tier. Theater ratio (0.48): Moderate-low. The barbell economy has real functional content — AI does amplify elite talent, compute is genuinely scarce, training does require substantial investment. The theater is not performative maintenance of a degraded system (that would be higher theater), but rather the performative maintenance of meritocracy narratives despite structural invalidity. The ratio would be lower (0.3-0.35) if the redistribution debate were absent, but the presence of contested solutions and cultural negotiation about inequality adds theater. Suppression (0.62): High. Barriers to median-worker exit are severe: retraining costs, age discrimination, geographic immobility, credential inflation, opportunity costs during transition. The suppression is structural, not primarily coercive, but equally effective. Claimed type: Snare. The constraint meets snare thresholds: high extractiveness (0.58), high suppression (0.62), high effective extraction for trapped agents (chi >> 0.66 at powerless/trapped index). The constraint persists not through coordination benefit but through capital concentration and access gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival bifurcation of the barbell economy. The elite talent (institutional/arbitrage) genuinely experiences Rope: AI is a coordination technology that enables their productivity. The capital owners (institutional/arbitrage) also experience Rope: they benefit from scarcity rent on compute and model access. The median worker (powerless/trapped) experiences pure Snare: maximum extraction with no exit. The displaced cognitive labor (moderate/constrained) experiences Snare with slightly more agency: some mobility options exist (geographic relocation, sector transition) but barriers are severe. The high-touch service sector (moderate/mobile) experiences Tangled Rope: genuine supply scarcity creates wage protection but rising credential barriers and gatekeeping create extraction features. The redistribution coalition (organized/constrained) experiences Scaffold: they see the barbell as a temporary coordination failure with a sunset — UBI or education restructuring could dissolve the barbell by democratizing AI access or replacing the credential hierarchy. The meritocracy narrative (institutional/arbitrage) persists as Piton: the story of talent-based outcomes is maintained through constant institutional effort despite its structural invalidity, creating the theater that legitimates the inequality. The analytical observer risks seeing Mountain: 'technological leverage inherently creates bimodal distributions' — but the structural data reveals this as a false summit; the barbell is sustained by contingent institutional arrangements (access gatekeeping, training concentration, credential hierarchies) not by physical or logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary dramatically across the labor market. Elite talent beneficiaries with arbitrage options derive low d (0.15-0.25) — they experience negative effective extraction (they are subsidized by the system). Trapped median workers derive high d (0.90-0.98) — they experience maximum extraction from the constraint. The sigmoid f(d) amplifies this gap: beneficiaries' low d maps to negative f(d) (-0.12 to -0.01), reducing their apparent chi; victims' high d maps to high f(d) (1.2-1.42), amplifying their experienced extraction. Displaced cognitive labor with constrained mobility derive intermediate-high d (0.75-0.85), experiencing strong extraction with some agency. Capital owners derive very low d (0.0-0.10) because their arbitrage options are abundant and the constraint directly subsidizes them. This d structure reveals the fundamental asymmetry: the constraint's extractiveness (0.58) is a base property, but effective extraction (χ) is highly heterogeneous across the population. The beneficiaries experience the system as beneficial coordination; the trapped experience it as pure extraction. The constraint's power to suppress alternatives (suppression = 0.62) is what holds the barbell in place despite the stark perspectival divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing genuine coordination function (AI amplifies elite productivity — real) from extraction (barbell structure suppresses median-worker alternatives — real). The Rope perspective (elite talent, capital owners) is not false; the Snare perspective (trapped workers) is not false. Both are structurally accurate. The mandatrophy is resolved by recognizing that the same technical system (AI leverage) functions as coordination for those with access and as extraction for those without. The false summit risk is the Mountain perspective: 'bimodal distributions are inherent to technological change.' The structural data contradicts this: the barbell is sustained by contingent institutional choices (who controls AI access, how training is gatekept, whether redistribution is funded). A different institutional arrangement (open-source models, universal AI training, wealth redistribution) would produce different outcomes without changing the underlying technology. The Scaffold perspective (sunset mechanisms) is not false but may be politically infeasible — that's an omega variable, not a mandatrophy issue. The meritocracy narrative is indeed Piton: it performs legitimacy while the underlying structural basis has collapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_training_accessibility_threshold,
    'At what cost and complexity threshold does AI tool accessibility shift from elite gatekeeping to genuinely commodified?',
    'Market analysis of API pricing, model parameter counts required for useful tasks, compute cost curves; comparison with historical technology adoption curves (SQL databases, cloud computing, spreadsheet software)',
    'If threshold drops to near-zero (open models, low-cost inference): barbell may be temporary coordination problem (Scaffold perspective gains strength). If threshold remains high (proprietary models, compute scarcity): barbell locks in as permanent extraction structure (Snare perspective confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ai_training_accessibility_threshold, empirical, 'Cost and complexity threshold for AI tool accessibility').

omega_variable(
    skill_substitution_vs_complementarity,
    'Does AI primarily substitute for median-skill cognitive labor or complement it?',
    'Labor economics analysis: wage changes by skill decile; job displacement vs job creation by sector; productivity gains vs wage gains by skill level; cross-national comparison of labor markets with different AI adoption rates',
    'If substitution dominates: barbell structure deepens (Snare classification confirmed). If complementarity dominates: median workers gain leverage and mobility (Tangled Rope classification gains strength, Snare weakens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_substitution_vs_complementarity, empirical, 'Whether AI substitutes for or complements median-skill labor').

omega_variable(
    talent_identification_mechanism_stability,
    'How stable are the mechanisms that identify ''elite talent'' in an AI-native economy?',
    'Analysis of elite identification pathways: educational credentials, test scores, prior work experience, network effects; comparison with actual performance in AI-native roles; identification of selection bias and false positives in talent identification',
    'If identification is highly stable and predictive: barbell structure locks in early (Mountain perspective gains force). If identification is unstable and subject to measurement error: alternative talent pools may be misclassified as low-tier (Scaffold perspective: UBI or education restructuring could reveal hidden talent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_identification_mechanism_stability, empirical, 'Stability of elite talent identification mechanisms').

omega_variable(
    redistribution_feasibility_and_political_viability,
    'Can redistributive mechanisms (UBI, wealth taxes, job guarantees) sustainably address barbell inequality or do they face structural political failure?',
    'Policy analysis: historical attempts at redistribution (Nordic models, guaranteed employment, basic income pilots); political economy of wealth concentration; dynamics of elite tax avoidance; feedback loops between inequality and political influence',
    'If redistribution is politically feasible: Scaffold perspective is structural (sunset mechanisms can work). If redistribution is politically blocked: Snare perspective is confirmed (no real exit for trapped cohort, redistribution is performative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redistribution_feasibility_and_political_viability, preference, 'Political feasibility of redistributive responses to barbell economy').

omega_variable(
    human_labor_scarcity_value_preservation,
    'In which sectors do human labor scarcity genuinely preserve high wage floors, and in which sectors does scarcity merely increase cost with limited compensation growth?',
    'Sectoral analysis: wage growth rates in AI-resistant sectors; cost pass-through to consumers; labor supply elasticity; barriers to apprenticeship and credential acquisition; geographic wage variation',
    'If scarcity genuinely preserves value: high-touch service sector (Tangled Rope perspective) creates real exit route. If cost scarcity without compensation: skilled trades experience squeeze (Snare features creep into Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_labor_scarcity_value_preservation, empirical, 'Whether labor scarcity in AI-resistant sectors preserves wage floors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(average_is_over_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aio_tr_t0, average_is_over_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(aio_tr_t3, average_is_over_2026, theater_ratio, 3, 0.43).
narrative_ontology:measurement(aio_tr_t6, average_is_over_2026, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(aio_be_t0, average_is_over_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aio_be_t3, average_is_over_2026, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aio_be_t6, average_is_over_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(average_is_over_2026, resource_allocation).
narrative_ontology:affects_constraint(average_is_over_2026, skill_premium_concentration).
narrative_ontology:affects_constraint(average_is_over_2026, education_credentialism_spiral).
narrative_ontology:affects_constraint(average_is_over_2026, capital_labor_substitution).

% DUAL FORMULATION NOTE:
% The AI-talent barbell economy is a unified structural phenomenon with multiple perspectives, not a decomposed constraint family. The extractiveness value reflects the actual income compression and labor-market bifurcation observed in 2026. Alternative formulations measuring only 'AI adoption rate' or 'talent scarcity' would produce different epsilon values and should be written as separate constraint stories if their measurement basis differs significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(average_is_over_2026, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
